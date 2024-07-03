{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- | Set of tests for our concurrent GC, run against a simple model of the
   reduction core.

   These tests will take a _long_ time to run unless you modify the `HeapSize`
   parameter in `Heron.Parameters` to something small (~16 or so). Make sure to
   also set `MaxFnAps` to something small too (~2 or so).
-}
module Heron.Tests.Core.Collector where

import           Data.Functor                ((<&>))
import           Data.List                   (delete, findIndices, intersect,
                                              partition, sort, (\\))
import           Prelude
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.TH

import           Hedgehog                    (Gen, Property, PropertyT,
                                              footnote, forAll, property,
                                              withTests, (===))
import           Hedgehog.Gen                (bool, choice, element, int, just)
import           Hedgehog.Range              (linear)

import           Control.Monad
import           Control.Monad.State

import           Clash.Hedgehog.Sized.Vector
import           Clash.Prelude               (Div, Generic, KnownNat, NFDataX,
                                              RamOp (..), SNat (..), ShowX,
                                              Vec (..), errorX, maybeHasX,
                                              showX, snatToNum, toList)
import qualified Clash.Sized.Vector          as V
import           Control.DeepSeq             (NFData)
import           Data.Maybe
import           Heron.Core.Collector
import           Heron.Core.Fifo             (FOut (..))
import           Heron.Core.Heap             (HeapOut (..))
import           Heron.Parameters
import           Heron.Template
import           RetroClash.Barbies

import           GHC.Stack

maxCycles :: HasCallStack => Int
maxCycles = 128 -- Make sure this is large enough to allow multiple GC passes

checkX :: (NFData a, NFDataX a) => String -> a -> a
checkX msg = fromMaybe (errorX msg) . maybeHasX

type Heap = [HeapNode]

ptrs :: HasCallStack => [Atom] -> [HeapAddr]
ptrs = mapMaybe heapAddr

atoms :: HasCallStack => HeapNode -> [Atom]
atoms (Case _ _ as) = catMaybes $ toList as
atoms (App  _ _ as) = catMaybes $ toList as
atoms (Prim _ _ as) = catMaybes $ toList as

infixl 9  !!!
(!!!) :: [a] -> HeapAddr -> a
(!!!) xs i = xs !! fromIntegral i

data MutState = MutState
  { roots  :: [HeapAddr]
  , heap   :: [HeapNode]
  , books  :: [GCNode]
  , muts   :: [HeapNode]
  , prevR  :: GCRequest
  , gcIn   :: Pure GCIn
  , cycles :: Integer
  }
  deriving (Show, Generic, NFDataX, ShowX)

-- * Reachability analysis

emptyMap :: HasCallStack => Heap -> [Bool]
emptyMap = map (const False)

setAt :: HasCallStack => HeapAddr -> a -> [a] -> [a]
setAt i a ms = xs ++ (a : tail ys)
  where (xs, ys) = splitAt i' ms
        i' = fromIntegral i

mark :: HasCallStack => Heap -> HeapAddr -> [Bool] -> [Bool]
mark h i ms
  -- Already marked; end.
  | ms !!! i = ms
  -- Mark this and any children
  | otherwise = foldr (mark h) (setAt i True ms) (ptrs . atoms $ h !!! i)

deallocCandidates :: HasCallStack => MutState -> [HeapAddr]
deallocCandidates MutState {..} =
  filter isCandidate $ take (snatToNum (SNat @CMaxPush)) roots
  where
    isCandidate a =
      let otherRoots = filter (a/=) roots
          ms = foldr (mark heap) (emptyMap heap) otherRoots
      in not $ ms !!! a

-- | Partition the address space into ([reachable], [unreachable]) addresses.
partitionHeapAddrs :: HasCallStack => MutState -> ([HeapAddr], [HeapAddr])
partitionHeapAddrs MutState{..} = both (map snd) . partition fst $ zip ms [0..]
  where
  ms = foldr (mark heap) (emptyMap heap) roots

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)

reachables :: HasCallStack => MutState -> [HeapAddr]
reachables = fst . partitionHeapAddrs

unreachables :: HasCallStack => MutState -> [HeapAddr]
unreachables = snd . partitionHeapAddrs

isFree :: HasCallStack => GCNode -> Bool
isFree (FreeList _) = True
isFree _            = False

isWork :: HasCallStack => GCNode -> Bool
isWork (WorkQueue _) = True
isWork _             = False

-- * Mutator state and operation generators

genHeapAddr :: HasCallStack => Gen HeapAddr
genHeapAddr = fromIntegral <$> int (linear 0 $ snatToNum (SNat @HeapSize) - 1)

genThreshold :: HasCallStack => Gen HeapAddr
genThreshold = fromIntegral <$> int (linear 0 $ snatToNum (SNat @(HeapSize `Div` 4)) - 1)

genAtom :: HasCallStack => [HeapAddr] -> Gen Atom
genAtom as = choice [pure $ PrimInt 0, Ptr True <$> element as]
  -- ^ We only generate pointers or one form of non-pointers

genApp :: HasCallStack => (KnownNat a, KnownNat b) => [HeapAddr] -> Gen (Node a b)
genApp as = do
  es <- genVec (choice [Just <$> genAtom as, pure Nothing])
  -- the `maybe` generator gives `Nothing` less frequently than I'd like.
  pure $ App False (len es) (sortMaybes es)
  -- ^ We only generate plain applications
  where
    len ms = V.fold (+) . V.map (maybe 0 (const 1)) $ Nothing V.:> ms

sortMaybes :: HasCallStack => KnownNat n => V.Vec n (Maybe a) -> V.Vec n (Maybe a)
sortMaybes = V.vfold ins
  where
    ins _ Nothing xs  = xs V.++ V.singleton Nothing
    ins _ (Just a) xs = Just a V.:> xs


-- A reasonable starting mutator state. A random subset of a random heap are
-- selected as roots.
genInitMutState :: HasCallStack => Gen MutState
genInitMutState = do
  let addrs = [0 ..]
      muts  = []
      books = map (const Unmarked) addrs
      request   = RNothing
      prevR     = RNothing
      gcMemIn = HeapOut 0 0 (V.repeat Unmarked)
      heapMemIn = Nothing
      bubble    = False
      updateIn  = FOut 0 Nothing
      cycles    = 0
      stkIn     = (0, PrimInt 0)
      roots     = []
  heap         <- replicateM (snatToNum $ SNat @HeapSize) (genApp addrs)
  triggerThres <- genThreshold
  let gcIn = GCIn {..}
  pure $ MutState {..}

initGCOut :: HasCallStack => GCState -> Pure GCOut
initGCOut s = GCOut
  { _gcMemOut   = V.repeat RamNoOp
  , _heapMemOut = Nothing
  , _nextFree   = V.map (both (Just . _llHead)) (_freelist s)
  , _cmd        = WaitCmd
  , _updatePop  = False
  , _stkSnoopAddr = 0
  , _remaining  = (0,0) :> (0,0) :> Nil
  }

genInit :: HasCallStack => Gen (MutState, GCState, Pure GCOut)
genInit = do
  i <- genInitMutState
  pure (i,initGCState,initGCOut initGCState)

genTopRoots :: HasCallStack => MutState -> Gen HeapAddr
genTopRoots = element . roots
  -- Our circuit really only mutates the top CMaxPush stack elements... the rule
  -- below would be more true to the implementation, but also (needlessly?) more
  -- strict.
  -- > element . take (snatToNum (SNat @CMaxPush)) . roots

genStep :: HasCallStack => MutState -> Pure GCOut -> Gen MutState
genStep i o = just $ choice go
  where
    nonSharedRoots = deallocCandidates i
    lives = reachables i
    stepNoOp = Just . genStepNoOp i o <$> bool
    stepRoots = Just . genStepRoots i o <$> bool
    stepReduce
      | null (roots i) = pure Nothing
      | otherwise      = genStepReduce i o <$> bool <*> genTopRoots i <&> Just
    stepUnwind
      | null (roots i) = pure Nothing
      | otherwise      = genStepUnwind i o <$> bool <*> genTopRoots i <&> Just
    stepDealloc
      | null nonSharedRoots = pure Nothing
      | otherwise           = genStepDealloc i o <$> bool <*> element nonSharedRoots <&> Just
    stepAlloc
      | bubble (gcIn i) = pure Nothing
      | otherwise       = do b <- bool
                             as <- genVec bool
                             Just <$> genStepAlloc i o genAlloc b as
    stepMutate
      | null lives = pure Nothing
      | otherwise  = genStepMutate i o <$> bool <*> genApp lives <*> element lives <&> Just
    stepWideMutate
      | null lives || bubble (gcIn i) = pure Nothing
      | otherwise = do b <- bool
                       as <- genVec bool
                       app <- genApp lives
                       u   <- element lives
                       let i' = genStepMutate i o False app u
                       i'' <- genStepAlloc i' o genAlloc b as
                       pure $ Just i''

    go | _cmd o == RootsCmd
       = [stepRoots]
       | _cmd o == NoCmd || _cmd o == UpdateBarrierCmd
       = [stepNoOp
         ,stepReduce
         ,stepDealloc
         ,stepUnwind
         ,stepAlloc
         ,stepMutate
         ,stepWideMutate
         ]
       | otherwise
       = [stepNoOp]

doMemOps :: HasCallStack => Pure GCOut -> MutState -> GCState -> MutState
doMemOps o i _ = i { gcIn = (gcIn i) { gcMemIn = memIn, heapMemIn = heapIn, updateIn = mutIn, stkIn = stkSnoop }
                   , books = books'
                   , muts = muts'
                   , prevR = request (gcIn i)
                   }
  where
    memIn = HeapOut 0 0 $ V.map gcRd (_gcMemOut o)
    gcRd (RamRead a   ) = books i !!! a
    gcRd (RamWrite a _) = books i !!! a
    gcRd RamNoOp        = Unmarked -- errorX $ unwords ["No address read: reading ", showX i, "\n  with outputs", showX o, "\n and state ", showX s]

    books' = foldl gcWr (books i) (V.toList $ _gcMemOut o)
    gcWr m (RamWrite a x) = setAt a x m
    gcWr m _              = m

    heapIn = (heap i !!!) <$> _heapMemOut o

    mutLen = fromIntegral $ length muts'
    mutIn  = FOut mutLen (listToMaybe muts')
    muts' | _updatePop o = case muts i of
                             []     -> []
                             (_:xs) -> xs
          | otherwise    = muts i

    stkSnoop = ( fromIntegral $ length (roots i)
               , let ptr = fromIntegral (_stkSnoopAddr o)
                 in if ptr < length (roots i)
                   then Ptr False $ reverse (roots i) !! ptr
                   else PrimInt 0
               )

-- Do nothing
genStepNoOp :: HasCallStack => MutState -> Pure GCOut -> Bool -> MutState
genStepNoOp i _ bubbleCycle = MutState
    { roots = roots i
    , heap  = heap i
    , books = books i
    , muts  = muts i
    , prevR = prevR i
    , cycles = cycles i
    , gcIn  = GCIn
      { gcMemIn      = gcMemIn $ gcIn i
      , heapMemIn    = heapMemIn $ gcIn i
      , updateIn     = updateIn  $ gcIn i
      , bubble       = bubbleCycle
      , triggerThres = triggerThres $ gcIn i
      , request      = RNothing
      , stkIn        = stkIn $ gcIn i
      }
    }

-- Gather roots
genStepRoots :: HasCallStack => MutState -> Pure GCOut -> Bool -> MutState
genStepRoots i _ bubbleCycle = MutState
  { roots = roots i
  , heap  = heap  i
  , books = books i
  , muts  = muts  i
  , prevR = prevR i
  , cycles = cycles i
  , gcIn  = GCIn
    { gcMemIn      = gcMemIn $ gcIn i
    , heapMemIn    = heapMemIn $ gcIn i
    , updateIn     = updateIn  $ gcIn i
    , bubble       = bubbleCycle
    , triggerThres = triggerThres $ gcIn i
    , request      = RRoot . V.unsafeFromList . take cmaxpush $ map (Ptr False) (roots i) ++ repeat (PrimInt 0)
    , stkIn        = stkIn $ gcIn i
    }
  }
  where
    cmaxpush = snatToNum (SNat @CMaxPush)

type Allocator m = [HeapAddr] -> ([HeapAddr],[HeapNode]) -> HeapAddr -> m ([HeapAddr],[HeapNode])

genAlloc :: HasCallStack => Allocator Gen
genAlloc lives (rs, h) a = do
  app <- genApp lives
  element
    [ (a : rs, setAt a app h) -- Alloc as a root
    , (rs    , setAt a app h) -- Alloc as heap app
    ]

-- When not stalled, take any amount of free addresses and allocate them
-- randomly between heap and roots.
genStepAlloc :: HasCallStack => Monad m => MutState -> Pure GCOut -> Allocator m -> Bool -> Vec MaxAps Bool -> m MutState
genStepAlloc i (GCOut {..}) f bubbleCycle as = do
      let addrs = catMaybes . toList $ V.zipWith3 fetch prevAllocs as _nextFree
          lives = addrs ++ reachables i
      (rs, h) <- foldM (f lives) (roots i, heap i) addrs
      pure $ MutState
        { roots = rs
        , heap  = h
        , books = books i
        , muts  = muts i
        , prevR = prevR i
        , cycles = cycles i
        , gcIn  = GCIn
          { gcMemIn      = gcMemIn   $ gcIn i
          , heapMemIn    = heapMemIn $ gcIn i
          , updateIn     = updateIn  $ gcIn i
          , bubble       = bubbleCycle
          , triggerThres = triggerThres $ gcIn i
          , request      = RAlloc as
          , stkIn        = stkIn $ gcIn i
          }
        }
  where
  prevAllocs = case prevR i of
    RAlloc xs -> xs
    _         -> V.repeat False
  fetch False True addrs = fst addrs
  fetch True  True addrs = snd addrs
  fetch _ False _        = Nothing


genStepDealloc :: HasCallStack => MutState -> Pure GCOut -> Bool -> HeapAddr -> MutState
genStepDealloc i _ bubbleCycle r = MutState
        { roots = filter (r/=) $ roots i
        , heap  = heap i
        , books = books i
        , muts  = muts i
        , prevR = prevR i
        , cycles = cycles i
        , gcIn  = GCIn
          { gcMemIn      = gcMemIn   $ gcIn i
          , heapMemIn    = heapMemIn $ gcIn i
          , updateIn     = updateIn  $ gcIn i
          , bubble       = bubbleCycle
          , triggerThres = triggerThres $ gcIn i
          , request      = RDealloc r
          , stkIn        = stkIn $ gcIn i
          }
        }

-- Do some useful reduction work (drop a root)
genStepReduce :: HasCallStack => MutState -> Pure GCOut -> Bool -> HeapAddr -> MutState
genStepReduce i _ bubbleCycle r = MutState
        { roots = delete r $ roots i
        , heap  = heap i
        , books = books i
        , muts  = muts i
        , prevR = prevR i
        , cycles = cycles i
        , gcIn  = GCIn
          { gcMemIn      = gcMemIn   $ gcIn i
          , heapMemIn    = heapMemIn $ gcIn i
          , updateIn     = updateIn  $ gcIn i
          , bubble       = bubbleCycle
          , triggerThres = triggerThres $ gcIn i
          , request      = RNothing
          , stkIn        = stkIn $ gcIn i
          }
        }

-- Unwind a root
genStepUnwind :: HasCallStack => MutState -> Pure GCOut -> Bool -> HeapAddr -> MutState
genStepUnwind i _ bubbleCycle r =
      let as = ptrs . atoms $ heap i !!! r
      in MutState
           { roots = (as ++) . delete r $ roots i
           , heap  = heap i
           , books = books i
           , muts  = muts i
           , prevR = prevR i
           , cycles = cycles i
           , gcIn  = GCIn
             { gcMemIn      = gcMemIn   $ gcIn i
             , heapMemIn    = heapMemIn $ gcIn i
             , updateIn     = updateIn  $ gcIn i
             , bubble       = bubbleCycle
             , triggerThres = triggerThres $ gcIn i
             , request      = RNothing
             , stkIn        = stkIn $ gcIn i
             }
           }

isRoots :: GCRequest -> Bool
isRoots (RRoot _ ) = True
isRoots _          = False

genStepMutate :: HasCallStack => MutState -> Pure GCOut -> Bool -> HeapNode -> HeapAddr -> MutState
genStepMutate i (GCOut {..}) bubbleCycle app n = MutState
  { roots = roots i
  , heap  = setAt n app $ heap i
  , books = books i
  , muts  = if _cmd == UpdateBarrierCmd
              then muts i ++ [heap i !!! n]
              else muts i
  , prevR = prevR i
  , cycles = cycles i
  , gcIn  = GCIn
    { gcMemIn      = gcMemIn   $ gcIn i
    , heapMemIn    = heapMemIn $ gcIn i
    , updateIn     = updateIn  $ gcIn i
    , bubble       = bubbleCycle
    , triggerThres = triggerThres $ gcIn i
    , request      = RNothing
    , stkIn        = stkIn $ gcIn i
    }
  }

-- * Simulation and properties

type Step = (MutState,GCState,Pure GCOut)
type GcStepProp a = Step -> Step -> PropertyT IO ()
type GcProp = [Step] -> PropertyT IO ()

allSteps :: HasCallStack => GcStepProp a -> GcProp
allSteps p xs = zipWithM_ p xs (tail xs)

scanM :: HasCallStack => (Monad m) => Int -> (s -> m s) -> s -> m [s]
scanM n f x = reverse <$> foldr (<=<) return (replicate n f') [x]
  where
    f' xs = f (head xs) <&> (: xs)

testGc :: HasCallStack => GcProp -> Property
testGc prop = property $ do
  (i, s, o) <- forAll genInit
  cycles <- forAll $ int (linear 1 maxCycles)
  steps <- scanM cycles tick (i, s, o)
  prop steps
  where
    tick (i,s,o) = do
      i' <- forAll $ genStep i o
      --evalIO $ putStrLn $ unlines ["GC Cycle ==================================== ", showX i', showX s]
      let (o', s') = runState (runGc $ gcIn i') s
          i'' = (doMemOps o' i' s') { cycles = cycles i' + 1 }
      --evalIO $ putStrLn $ unlines ["Post **************************************** ", showX s', showX o']
      pure (i'', s', o')
      -- input is old input state before next op is decided, old GC state and old output
      -- output is new input state (with memory reads), new GC state, and new output

works :: HasCallStack => MutState -> GCState -> [HeapAddr]
works i s
  | _llUpdate (_worklist s) &&
    _phase s /= Roots
  = llist i ((_worklist s) { _llHead = tailPtr latch })
  | otherwise
  = llist i (_worklist s)
  where
    latch = fromMaybe (checkX "test works" . V.head $ _reads (gcMemIn $ gcIn i)) (_workLatch s)

frees :: HasCallStack => MutState -> GCState -> [HeapAddr]
frees i s =
  concat [ concat . toList $ V.map (llist i . fst) (_freelist s)
         , concat . toList $ V.zipWith (\a f -> llist i $ hds a f)
             (checkX "test frees" . _reads . gcMemIn $ gcIn i)
             (V.map snd $ _freelist s)
         ]
  where
    hds a f
      | _llUpdate f = f {_llHead = tailPtr a}
      | otherwise   = f

llist :: HasCallStack => MutState -> GCList -> [HeapAddr]
llist s = go []
  where
    go xs l
      | _llLen l == minBound = xs
      | _llLen l == 1 = _llHead l : xs
      | otherwise = let hd = _llHead l
                        next = tailPtr (books s !!! hd)
                    in go (hd:xs) (l {_llLen = _llLen l - 1
                                     ,_llHead = next})

{-
llist only needs books memory from input

frees needs:
  + freelist ll from state
  + gcMemIn from output
-}

initialising :: HasCallStack => Phase -> Bool
initialising (Init _) = True
initialising _        = False

-- | All elements tagged as free must be found by traversal from the head
--   pointer. This ensures that the linked list has not been broken by
--   overwriting an element in its tail --- either by adding the same element
--   twice or by overwriting an element with a different tag.
prop_FreesLinked :: Property
prop_FreesLinked = withTests 10000 $ testGc $ allSteps prop
  where
    prop (_, _, _) (i', s', _) =
      unless (initialising $ _phase s') $ do
             sort (frees i' s') === map fromIntegral (findIndices isFree (books i'))

-- | Same as `prop_FreesLinked` but for the work list.
prop_WorksLinked :: Property
prop_WorksLinked = withTests 10000 $ testGc $ allSteps prop
  where
    prop (_, _, _) (i', s', _) =
      unless (initialising $ _phase s')
             ( footnote (unwords ["worklist <- ", showX (_worklist s'), "\n"
                                 ,"books <- ", showX (books i'), "\n"
                                 ,"muts  <- ", showX (muts i'), "\n"
                                 ,"workLatch <-", showX (_workLatch s'), "\n"
                                 ,"read <-", showX (gcMemIn $ gcIn i'), "\n"
                                 ]
                        ) >>
               sort (works i' s') === map fromIntegral (findIndices isWork (books i'))
             )


-- | All addresses suggested for allocation must not be reachable from the roots.
prop_FreesConsistent :: Property
prop_FreesConsistent = withTests 10000 $ testGc $ allSteps prop
  where
    prop (_, _, _) (i', s', _) =
      unless (initialising $ _phase s') $ do
             frees i' s' `intersect` reachables i' === []

-- | All unreachable data must be collected within two GC passes
prop_UnreachableLifetimes :: Property
prop_UnreachableLifetimes = withTests 50000 $ testGc (prop . filterInit)
  where
    filterInit = filter (\(_,s,_)-> not $ initialising $ _phase s)
    prop xs = do
      leaks === []
      where
        pairs = zip xs (tail xs)
        consec f y = zipWith f y (tail y)
        grouping :: [[a]] -> [(Int,a)]
        grouping = concat . zipWith (map . (,) ) passes

        -- Newly unreachable nodes generated at each step
        orphans = grouping . consec (flip (\\)) $
                  map (\(i,_,_) -> unreachables i) xs

        -- Nodes newly put on to the free list at each step
        freed   = grouping . consec (flip (\\)) $
                  map (\(i,s,_) -> frees i s) xs

        -- The GC pass number for each step
        startingPass ((_,s,_), (_,s',_))
          | _phase s  == Idle &&
            _phase s' == Roots = True
          | otherwise          = False
        passes  = scanl (\n s -> if startingPass s then n + 1 else n) 0 pairs
        lastPass = if null passes then 0 else last passes

        leaks =
          [ (pass, addr)
          | (pass, addr) <- orphans
          , null [(x,y) | (x,y) <- freed, x >= pass, x < pass + 2, y==addr]
            && pass <= lastPass - 2
          ]

tests :: HasCallStack => TestTree
tests = $(testGroupGenerator)

main :: HasCallStack => IO ()
main = defaultMain tests

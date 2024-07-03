{-# LANGUAGE RecordWildCards #-}

{-| A demonstative semantics for the garbage collector
-}
module Heron.Semantics.Collector
  (
  -- * GC model types
    GcIn(..)
  , GcState(..)
  , GcOut(..)
  , initialGcState
  -- * Simulation
  , runGc
  ) where

import           Control.Lens
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Maybe          (isNothing, mapMaybe)
import           Prelude             hiding (read)

-- * Expressions and memories

type Addr = Int
data Atom     = Ptr Addr | NonPtr
  deriving (Eq, Show)
type App      = [Atom]

ptr :: Atom -> Maybe Addr
ptr (Ptr x) = Just x
ptr NonPtr  = Nothing

ptrs :: App -> [Addr]
ptrs = mapMaybe ptr

-- * GC data structures

-- | Memory management requests from the mutator
data GcRequest
  = IAlloc  Int   -- ^ The top @n@ free addresses have been allocated
  | IFree    Addr -- ^ This address is now unreachable
  | INothing      -- ^ A no-op
  deriving Show

-- | Read-only mutator state exposed to the collector
data GcIn = GcIn
  { roots     :: [Addr]
  , heap      :: [App]
  , mutation  :: Maybe App
  , threshold :: Int
  , request   :: GcRequest
  }
  deriving Show

-- | Feedback to the mutator
data GcOut = GcOut
  { frees :: [Addr]
  , works :: [Addr] -- DEBUG
  , stall :: Bool
  }
  deriving Show

-- | States of the collector's FSM
data GcPhase
  = Idle
  | Roots
  | Mark
  | Sweep Addr
  deriving (Eq, Show)

-- | Collector bookkeeping entry
data GcNode
  = Free (Maybe Addr) -- ^ An entry on the linked list of free addresses
  | Work (Maybe Addr) -- ^ An entry of the marker's list of to-be-marked addresses
  | Unmarked          -- ^ A possibly unreachable address
  | Marked            -- ^ A definitely reachable address
  deriving (Show, Eq)

listTail :: GcNode -> Maybe Addr
listTail (Free x) = x
listTail (Work x) = x
listTail _        = Nothing

-- | Collector's full state
data GcState = GcState
  { _phase    :: GcPhase
  , _mem      :: [GcNode]
  , _freeList :: Maybe Addr
  , _workList :: Maybe Addr
  }
  deriving Show
makeLenses ''GcState

-- | Initial GC state
initialGcState :: Int -> GcState
initialGcState n = GcState
  { _phase    = Idle
  , _mem      = take n $ Free Nothing
                       : map (Free . Just) [0 ..]
  , _freeList = Just (n-1)
  , _workList = Nothing
  }

type Gc a = State GcState a

-- * GC state helpers

read  :: Addr -> Gc GcNode
read i = use mem <&> (!! i)

write :: Addr -> GcNode -> Gc ()
write i a = mem %= replace
  where
    replace m =
      let (xs, ys) = splitAt i m
      in  xs ++ a : tail ys

pop :: Lens' GcState (Maybe Addr) -> Gc Addr
pop hd = do
  use hd >>= \case
    Nothing -> error "Cannot pop from empty list"
    Just a  -> do
      m <- use mem
      hd .= listTail (m !! a)
      pure a

list :: [GcNode] ->  Maybe Addr -> [Addr]
list _ Nothing  = []
list m (Just a) = a : list m (listTail $ m !! a)

len :: Lens' GcState (Maybe Addr) -> Gc Int
len hd = do
  m <- use mem
  h <- use hd
  pure . length $ list m h

push :: Lens' GcState (Maybe Addr) -> (Maybe Addr -> GcNode) -> Addr -> Gc ()
push hd n x = do
  ma <- use hd
  hd .= Just x
  write x (n ma)

enqueueWork :: [Addr] -> Gc ()
enqueueWork = mapM_ enQ
  where
    enQ a =
      whenM (read a <&> (==Unmarked))
              (push workList Work a)

marking :: GcPhase -> Bool
marking = (==) Mark

unswept :: Addr -> GcPhase -> Bool
unswept x (Sweep y) | x >= y = True
unswept _ _                  = False

-- | Represents one big step in the (ideal) operational semantics.
-- | Chooses step behaviour based on mutator's `GcRequest`.
step :: GcIn -> Gc ()
step GcIn {..} = case request of
  -- Only progress GC tasks when there's no request
  INothing -> use phase >>= progress (GcIn {..})
  -- Handle immediate free requests

  IFree x  ->
    -- Frees are signalled at the exact time they fall out of scope, so we can
    -- guarantee that there are not already on the free list. When marking, we
    -- might free something before it is marked! So, unless we want to patch up
    -- the freelist during marking (no! since we are not doubly linked), we need
    -- to just ignore frees during marking.
    unlessM (use phase <&> marking)
            (push freeList Free x)

  -- Handle allocation notifications
  IAlloc n ->
    replicateM_ n alloc
    where
      alloc = do
        a <- pop freeList
        p <- use phase
        write a $ if marking p || unswept a p
                    then Marked else Unmarked

-- | Progress background GC tasks based on the current `GcPhase`
progress :: GcIn -> GcPhase -> Gc ()

-- Trigger GC tasks once # free addresses falls below threshold
progress (GcIn {..}) Idle = do
  remaining <- len freeList
  when (remaining <= threshold)
       (phase .= Roots)

-- Take stable snapshot of graph roots before marking.
progress (GcIn {..}) Roots = do
  enqueueWork roots
  phase .= Mark

-- Mark all (possibly) live data while adding their children to the work list.
progress i Mark =
  case mutation i of
    Just as -> enqueueWork $ ptrs as
    Nothing -> use workList >>= \case
      Nothing -> phase .= Sweep 0
      Just  _ -> do
        w <- pop workList
        write w Marked
        enqueueWork (ptrs $ heap i !! w)
        -- ^ We used to need to check that `w` was not Free before Marking.
        -- Since we now disable immediate frees during marking, we know that can
        -- never be the case. Also note, this is the _only_ place the GC needs
        -- access to the mutator's heap.

-- Sweep through all addresses, resetting all marked data back to unmarked, and
-- pushing unmarked ptrs onto the free list.
progress i (Sweep n) = do
  phase .= if endOfHeap then Idle else Sweep (n+1)
  read n >>= \case
    Marked   -> write n Unmarked
    Unmarked -> push freeList Free n
    _        -> pure ()
  where
    endOfHeap = n + 1 == length (heap i)

gcOut :: GcState -> GcOut
gcOut s = GcOut
  { frees = list (_mem s) (_freeList s)
  , works = list (_mem s) (_workList s) -- DEBUG
  , stall = isNothing $ _freeList s
  }

-- | Run collector semantics for one cycle
runGc :: GcIn -> GcState -> (GcState, GcOut)
runGc i s = (s', gcOut s')
  where
    s' = execState (step i) s

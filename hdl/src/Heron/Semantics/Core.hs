-- | A concise, high-level implementation of the Heron Core semantics. This is true
--    largely to the hardware implementation, excluding hardware-dependent quirks,
--    such as heap stall cycles when no free ports are available. (Such quirks
--    /are/ implemented in the C emulator.)
module Heron.Semantics.Core
  ( -- * Memory data types
    Heap,
    HeapAddr,
    Stack,
    StackAddr,
    UStack,
    AStack,
    PStack,
    Regs,
    State,

    -- * Semantics
    step,
    run,

    -- * Debugger helpers
    noDebug,
    debugWhen,
    debugWithGraphviz,
    debugWithLog,

    -- * Testing
    checkAll,
  )
where

import           Clash.Prelude                     (SNat (..), snatToNum)
import           Control.Monad                     (void)
import           Data.Bifunctor                    (second)
import           Data.Graph.Inductive.Graph        (mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (GlobalAttributes (..),
                                                    GraphID (..),
                                                    GraphvizOutput (..),
                                                    Labellable,
                                                    NodeCluster (..), clusterBy,
                                                    clusterID, defaultParams,
                                                    fmtCluster, fmtEdge,
                                                    fmtNode, graphToDot,
                                                    runGraphviz, toLabel,
                                                    toLabelValue)
import           Data.GraphViz.Attributes.Complete (Attribute (..),
                                                    RankDir (..))
import           Data.List                         (isPrefixOf)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text.Lazy                    as L (pack)
import           Flite.TemplateSyntax              (App (..), Atom (..),
                                                    LUT (..), Prog, Shared,
                                                    isFUN)
import           Heron.Encode                      ()
import           Heron.External                    (compileBenchmark,
                                                    getProjectFile, runEmulator)
import           Heron.Parameters                  (MaxRegs)
import           Prelude

-- | Heap memory
type Heap = [App]

-- | Heap Pointer
type HeapAddr = Int

-- | Primary stack
type Stack = [Atom]

-- | Stack pointer
type StackAddr = Int

-- | Stack of update pointers
type UStack = [(StackAddr, HeapAddr)]

-- | Stack of case tables
type AStack = [LUT]

-- | Stack of primitives
type PStack = [Int]

-- | Primitive registers
type Regs = [Atom]

-- | Full system state
type State = (Stack, Prog, Heap, UStack, AStack, Regs, Stack, PStack, [HeapAddr], [HeapAddr])

-- | Debugger functions
type Debugger = Integer -> State -> IO ()

-- Helpers

dash :: Shared -> Atom -> Atom
dash True (VAR _ n) = VAR True n
dash True (ARG _ n) = ARG True n
dash True (REG _ n) = REG True n
dash _ a            = a

dashs :: Shared -> [Atom] -> [Atom]
dashs sh = map (dash sh)

write :: [a] -> Int -> a -> [a]
write as i a = take i as ++ [a] ++ drop (i + 1) as

arity :: Atom -> Int
arity (FUN _ ar _) = ar
arity (PRI ar _)   = ar
arity (INT _)      = 1
arity (CON ar _)   = 1 + ar
arity _            = 0

boolAtom :: Bool -> Atom
boolAtom True  = CON 0 1
boolAtom False = CON 0 0

alu :: String -> Int -> Int -> Atom
alu op a b
  | swap = go op' b a
  | otherwise = go op' a b
  where
    swap = "swap:" `isPrefixOf` op
    op' = if swap then drop 5 op else op

    go "(+)" x y = INT $ x + y
    go "(-)" x y = INT $ x - y
    go "(==)" x y = boolAtom $ x == y
    go "(/=)" x y = boolAtom $ x /= y
    go "(<=)" x y = boolAtom $ x <= y
    go primOp _ _ = error $ "Heron.Semantics.alu: Unsupported primitive operation: " ++ primOp

splitApp :: App -> (Bool, [LUT], [Atom])
splitApp (APP nf es) = (nf, [], es)
splitApp (CASE c es) = (False, [c], es)
splitApp (PRIM _ _)  = error "splitApp: PRIM is a special case"

instAtom ::
  -- | Previously allocated addresses
  [HeapAddr] ->
  -- | Next free addresses
  [HeapAddr] ->
  -- | Stack contents
  Stack ->
  -- | Heap contents
  Heap ->
  -- | PRS Register contents
  Regs ->
  -- | Atom to instantiate
  Atom ->
  Atom
instAtom _ _ stk _ _ (ARG sh n) = dash sh (stk !! n)
instAtom _ _ _ _ regs (REG sh n) = dash sh (regs !! n)
instAtom aOld aNext _ _ _ (VAR sh n)
  | n >= 0 = VAR sh (aNext !! n)
  | otherwise = VAR sh (aOld !! negate (n + 1))
instAtom _ _ _ _ _ a = a

instAtoms ::
  -- | Previously allocated addresses
  [HeapAddr] ->
  -- | Next free addresses
  [HeapAddr] ->
  -- | Stack contents
  Stack ->
  -- | Heap contents
  Heap ->
  -- | PRS Register contents
  Regs ->
  -- | Atoms to instantiate
  [Atom] ->
  [Atom]
instAtoms aOld aNext stk heap regs = map (instAtom aOld aNext stk heap regs)

instApp ::
  -- | Previously allocated addresses
  [HeapAddr] ->
  -- | Next free addresses
  [HeapAddr] ->
  -- | Stack contents
  Stack ->
  -- | Heap contents
  Heap ->
  -- | Accumulated new applications and register states
  ([App], Regs) ->
  -- | Application to instantiate
  App ->
  ([App], Regs)
instApp ao an stk heap (us, regs) (APP nf es) =
  let u' = APP nf (instAtoms ao an stk heap regs es)
   in (us ++ [u'], regs)
instApp ao an stk heap (us, regs) (CASE c es) =
  let u' = CASE c (instAtoms ao an stk heap regs es)
   in (us ++ [u'], regs)
instApp ao an stk heap (us, regs) (PRIM n es) =
  let es' = instAtoms ao an stk heap regs es
   in case es' of
        [INT n0, INT n1, PRI _ op] ->
          (us, write regs n (alu op n0 n1))
        _ ->
          (us ++ [APP False es'], write regs n (VAR False (an !! length us)))

newChain :: Atom -> Atom
newChain (FUN _ a n) = FUN True a n
newChain a           = a

-- Transitions

-- | A single step of the operational semantics. We don't account for hardware
-- bounds here --- the real implementation will occasionally stall when
-- prefetching from heap.
step :: State -> State
-- Unwind
step (VAR s n : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an) =
  (dashs s es ++ stk, p, h, us ++ ustk, cs ++ cstk, regs, frz, pstk, ao, an)
  where
    (isNF, cs, es) = splitApp (h !! n)
    us = [(length stk, n) | s && not isNF]

-- Update
step (e : stk, p, h, (sp, n) : ustk, cstk, regs, frz, pstk, ao, an)
  | arity e > length stk - sp =
      (es1' ++ es2, p, h', ustk, cstk, regs, frz, pstk, ao, an)
  where
    (es1, es2) = splitAt (arity e) (e : stk)
    es1' = dashs True es1
    h' = write h n (APP True es1)

-- Primitives
step (INT n : PRI _ "(!)" : e : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an) =
  (e : INT n : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an)
step (INT n0 : INT n1 : PRI 2 op : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an) =
  (alu op n0 n1 : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an)
step (INT n1 : PRI 2 op : stk, p, h, ustk, cstk, regs, frz, n0 : pstk, ao, an) =
  (alu op n0 n1 : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an)
step (INT n0 : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an) =
  (stk, p, h, ustk, cstk, regs, frz, n0 : pstk, ao, an)
-- Case select
step (CON _ n : stk, p, h, ustk, c : cstk, regs, frz, pstk, ao, an)
  | isFUN e =
      step (e : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an)
  | otherwise =
      (stk', p, h, ustk, cstk, regs, frz, pstk, ao, an)
  where
    (d, e) = second newChain $ pickAlt n c

    pickAlt o (LOffset n') = (0, FUN True 0 (o + n'))
    pickAlt o (LInline as) = as !! o

    es = instAtoms ao an stk h regs [e]
    stk' = es ++ drop d stk

-- Unfold
step (FUN ft _ n : stk, p, h, ustk, cstk, regs, frz, pstk, ao, an) =
  (es' ++ drop a stk, p, h', ustk, cs ++ cstk, regs', frz', pstk, ao', an')
  where
    (_, a, cs, es, us) = p !! n
    frz' = if ft then stk else frz
    es' = instAtoms ao an frz' h regs es
    (us', regs') = foldl (instApp ao an frz' h) ([], regs) us
    acur = take (length us') an
    h' = foldl (\as (i, u) -> write as i u) h (zip acur us')
    an' = drop (length us') an
    ao' = reverse acur ++ ao
step s = error $ "Missing pattern in step: " ++ show s

-- | Run `step` until the program is fully evaluated. Gives a lower bound on the
--   cycles required by the hardware implementation (equivalent except
--   heap-prefetching stalls and GC)
run ::
  -- | Debugging routine
  Debugger ->
  -- | Input program
  Prog ->
  -- | (Cycle count, Return value)
  IO (Integer, Int)
run debug prog = do
  writeFile "/tmp/heron_sem.log" $ unlines (zipWith (curry show) [0 :: Integer ..] prog)
  eval 0 initialState
  where
    initialState = ([FUN True 0 0], prog, replicate 32768 (APP False []), [], [], replicate maxRegs (INT 0), [], [], [], [0 .. 32678])
    eval n s@([INT i], _, _, _, _, _, _, _, _, _) = debug n s >> pure (n, i)
    eval n s =
      debug n s
        >> eval (n + 1) (step s)
    maxRegs = snatToNum (SNat @MaxRegs)

-- | A placeholder debugger that is a no-op
noDebug :: Debugger
noDebug _ _ = return ()

-- | Guard a debugger with a boolean condition
debugWhen :: (Integer -> Bool) -> Debugger -> Debugger
debugWhen f debug n s
  | f n = debug n s
  | otherwise = return ()

-- | Capture the full textual state in `/tmp/heron_sem.log`
debugWithLog :: Debugger
debugWithLog n s = do
  appendFile "/tmp/heron_sem.log" $
    unlines
      [ "State on cycle " ++ show n,
        "======================================",
        showState s,
        "**************************************"
      ]
  where
    showState (stk, _, h, us, cs, _, _, ps, _, _) =
      unlines
        [ "  Stack | " ++ show stk,
          "  Heap  | " ++ showMem isEmptyApp h,
          "U Stack | " ++ showMem (const False) us,
          "C Stack | " ++ show cs,
          "P Stack | " ++ show ps
        ]
    isEmptyApp (APP _ []) = True
    isEmptyApp _          = False
    showMem f xs = unlines $ "" : map (\x -> "          " ++ show x)
                                      (filter (not . f . snd) $
                                       zip [0 :: Integer ..] xs)

-- | Display the graph state using Graphviz interactively.
debugWithGraphviz :: Debugger
debugWithGraphviz n s = do
  writeStateGraph (graphState s)
  putStrLn $ "Graph SVG written to /tmp/heron_state.svg for cycle " ++ show (n + 1) ++ "..."
  putStrLn "Type [Enter] for more..."
  _ <- getLine
  return ()

-- | Ensure that the semantics presented here give same result (in bounded
-- cycles) as the C emulator.
checkAll :: IO Bool
checkAll =
  do
    ok <- benchmarks >>= mapM check
    return $ and ok
  where
    check fname =
      do
        p <- compileBenchmark fname
        (emuRet, emuCycles) <- runEmulator p
        (cycles, ret) <- run noDebug p
        putStrLn $
          "Checking "
            ++ show fname
            ++ "... got "
            ++ show (cycles, ret)
            ++ " and expected "
            ++ show (emuCycles, emuRet)
        return $ cycles <= emuCycles && emuRet == fromIntegral ret

    benchmarks = mapM (getProjectFile . ("tests/benchmarks/" ++))
      [ "adjoxo.fl",
        "braun.fl",
        "cichelli.fl",
        "clausify.fl",
        "countdown.fl",
        "fib.fl",
        "knuthbendix.fl",
        "mate.fl",
        "mss.fl",
        "ordlist.fl",
        "permsort.fl",
        "queens2.fl",
        "queens.fl",
        "sumpuz.fl",
        "taut.fl",
        "while.fl"
      ]

--------------------------------------------------------------------------------
-- State visualisation

writeStateGraph :: Gr GNode String -> IO ()
writeStateGraph g = void (runGraphviz dg Svg "/tmp/heron_state.svg")
  where
    dg = graphToDot params g
    params =
      defaultParams
        { fmtNode = \(_, l) -> [toLabel l],
          fmtEdge = \(_, _, l) -> [toLabel l],
          clusterID = clusters,
          clusterBy = toCluster,
          fmtCluster = clusterFmts
        }

    clusters CHeap = Str $ L.pack "heap"
    clusters CVStk = Str $ L.pack "vstk"
    clusters CUStk = Str $ L.pack "ustk"
    clusters CAStk = Str $ L.pack "astk"
    clusters CPStk = Str $ L.pack "pstk"
    clusters CReg  = Str $ L.pack "regs"

    toCluster n@(_, GHeap _ _) = C CHeap $ N n
    toCluster n@(_, GVStk _ _) = C CVStk $ N n
    toCluster n@(_, GUStk _ _) = C CUStk $ N n
    toCluster n@(_, GAStk _ _) = C CAStk $ N n
    toCluster n@(_, GPStk _ _) = C CPStk $ N n
    toCluster n@(_, GReg _ _)  = C CReg $ N n

    clusterFmts CVStk = [GraphAttrs [RankDir FromRight]]
    clusterFmts _     = []

data GNode
  = GHeap Int App
  | GVStk Int Atom
  | GUStk Int (StackAddr, Int)
  | GAStk Int LUT
  | GPStk Int Int
  | GReg Int Atom
  deriving (Show, Eq)

instance Labellable GNode where
  toLabelValue = toLabelValue . show

data GCluster
  = CHeap
  | CVStk
  | CUStk
  | CAStk
  | CPStk
  | CReg
  deriving (Show, Eq, Ord)

graphState :: State -> Gr GNode String
graphState (vs, _, hs, us, as, rs, _, ps, _, _) =
  mkGraph nodes edges
  where
    nodes =
      filter (not . garbage)
        . zip [0 ..]
        $ zipWith GVStk [0 ..] vs
          ++ zipWith GHeap [0 ..] hs
          ++ zipWith GUStk [0 ..] us
          ++ zipWith GAStk [0 ..] as
          ++ zipWith GPStk [0 ..] ps
          ++ zipWith GReg [0 ..] rs
    nodesF = map (\(i, n) -> (n, i)) nodes
    nodeIndx n =
      fromMaybe maxBound $
        lookup n nodesF

    edges = concatMap findEdges nodes ++ vstkOrderEdges (zipWith const [0 ..] vs)

    findEdges (n, GHeap _ x) = appEdges n x
    findEdges (n, GVStk _ x) = atomEdges n x
    findEdges (n, GReg _ x)  = atomEdges n x
    findEdges _              = []

    atomEdges n (VAR _ addr) = [(n, nodeIndx $ GHeap addr (hs !! addr), "")]
    atomEdges n (REG _ addr) = [(n, nodeIndx $ GReg addr (rs !! addr), "")]
    atomEdges _ _            = []

    appEdges n (APP _ a)  = concatMap (atomEdges n) a
    appEdges n (CASE _ a) = concatMap (atomEdges n) a
    appEdges n (PRIM _ a) = concatMap (atomEdges n) a

    garbage (_, GHeap _ (APP _ [])) = True
    garbage _                       = False

    vstkOrderEdges xs = case xs of
      (x : y : rest) -> (x, y, "") : vstkOrderEdges (y : rest)
      _              -> []

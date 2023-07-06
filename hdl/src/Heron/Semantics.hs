{-| A concise, high-level implementation of the Heron Core semantics. This is true
    largely to the hardware implementation, excluding hardware-dependent quirks,
    such as heap stall cycles when no free ports are available. (Such quirks
    /are/ implemented in the C emulator.)
-}
module Heron.Semantics
  ( -- * Memory data types
    Heap
  , HeapAddr
  , Stack
  , StackAddr
  , UStack
  , AStack
  , Regs
  , State
    -- * Semantics
  , step
  , run
    -- * Testing
  , checkAll
  ) where

import Prelude
import Data.List
import Flite.TemplateSyntax
import Heron.External
import Heron.Template (MaxRegs)
import Clash.Prelude (SNat(..), snatToNum)

-- | Heap memory
type Heap      = [App]
-- | Heap Pointer
type HeapAddr  = Int
-- | Primary stack
type Stack     = [Atom]
-- | Stack pointer
type StackAddr = Int
-- | Stack of update pointers
type UStack    = [(StackAddr, HeapAddr)]
-- | Stack of case tables
type AStack    = [LUT]
-- | Primitive registers
type Regs      = [Atom]

-- | Full system state
type State = (Stack, Prog, Heap, UStack, AStack, Regs, Stack)

-- Helpers

dash :: Shared -> Atom -> Atom
dash True (VAR _ n) = VAR True n
dash True (ARG _ n) = ARG True n
dash True (REG _ n) = REG True n
dash _ a = a

dashs :: Shared -> [Atom] -> [Atom]
dashs sh = map (dash sh)

write :: [a] -> Int -> a -> [a]
write as i a = take i as ++ [a] ++ drop (i+1) as

arity :: Atom -> Int
arity (FUN _ arity _) = arity
arity (PRI arity   _) = arity
arity (INT arity)     = 1
arity (CON arity   _) = 1+arity
arity _ = 0

boolAtom :: Bool -> Atom
boolAtom True = CON 0 1
boolAtom False = CON 0 0

swap :: String -> String
swap op
  | swap = op'
  | otherwise = "swap:" ++ op
  where
    swap = "swap:" `isPrefixOf` op
    op' = if swap then drop 5 op else op

alu :: String -> Int -> Int -> Atom
alu op x y
  | swap      = go op' y x
  | otherwise = go op' x y
  where
    swap = "swap:" `isPrefixOf` op
    op' = if swap then drop 5 op else op

    go "(+)" x y = INT $ x + y
    go "(-)" x y = INT $ x - y
    go "(==)" x y = boolAtom $ x == y
    go "(/=)" x y = boolAtom $ x /= y
    go "(<=)" x y = boolAtom $ x <= y

splitApp :: App -> (Bool, [LUT], [Atom])
splitApp (APP nf es) = (nf, [], es)
splitApp (CASE c es) = (False, [c], es)
splitApp (PRIM _ _ ) = error $ "splitApp: PRIM is a special case"

instAtom stk heap regs (ARG sh n) = dash sh (stk  !! n)
instAtom stk heap regs (REG sh n) = dash sh (regs !! n)
instAtom stk heap regs (VAR sh n) = VAR sh (n + length heap)
instAtom _ _ _ a = a

instAtoms stk heap regs = map (instAtom stk heap regs)

instApp stk heap (us, regs) (APP nf es)
  = let u' = APP nf (instAtoms stk heap regs es)
    in (us++[u'], regs)
instApp stk heap (us, regs) (CASE c es)
  = let u' = CASE c (instAtoms stk heap regs es)
    in (us++[u'], regs)
instApp stk heap (us, regs) (PRIM n es)
  = let es' = instAtoms stk heap regs es
    in case es' of
         [INT n0, PRI _ op, INT n1] ->
           (us, write regs n (alu op n0 n1))
         _ ->
           (us++[APP False es'], write regs n (VAR False (length $ heap ++ us)))

newChain :: Atom -> Atom
newChain (FUN _ a n) = FUN True a n
newChain a = a

-- Transitions

-- | A single step of the operational semantics. We don't account for hardware
-- bounds here --- the real implementation will occasionally stall when
-- prefetching from heap.
step :: State -> State

-- Unwind
step (VAR s n     : stk, p, h,       ustk,       cstk, regs, frz)
  =  (dashs s es ++ stk, p, h, us ++ ustk, cs ++ cstk, regs, frz)
  where
    (isNF, cs, es) = splitApp (h !! n)
    us = if (s && not isNF) then [(length stk, n)] else []

-- Update
step (e     : stk, p, h , (sp,n) : ustk, cstk, regs, frz)
  | arity e > length stk - sp
  =  (es1' ++ es2, p, h',          ustk, cstk, regs, frz)
  where
    (es1, es2) = splitAt (arity e) (e:stk)
    es1'       = dashs True es1
    h' = write h n (APP True es1)

-- Primitives
step (INT n        : PRI _ "(!)"  : e               :  stk, p, h, ustk, cstk, regs, frz)
  =  (               e               : INT n        : stk, p, h, ustk, cstk, regs, frz)
step (INT n0       : PRI 2 op        : INT n1       : stk, p, h, ustk, cstk, regs, frz)
  =  (                                 alu op n0 n1 : stk, p, h, ustk, cstk, regs, frz)
step (INT n0       : PRI 2 op        : x            : stk, p, h, ustk, cstk, regs, frz)
  =  (x            : PRI 2 (swap op) : INT n0       : stk, p, h, ustk, cstk, regs, frz)

-- Case select
step      (CON _ n : stk , p, h, ustk, c:cstk, regs, frz)
  | isFUN e
  =  step (e       : stk , p, h, ustk,   cstk, regs, frz)
  | otherwise
  =       (          stk', p, h, ustk,   cstk, regs, frz)
  where
    (d, e) = (\(d,e) -> (d, newChain e)) (pickAlt n c)

    pickAlt n (LOffset n') = (0, FUN True 0 (n+n'))
    pickAlt n (LInline as) = as !! n

    es   = instAtoms stk h regs [e]
    stk' = es ++ drop d stk

-- Unfold
step (FUN ft _ n  : stk, p, h       , ustk,       cstk, regs , frz )
  =  (es' ++ drop a stk, p, h ++ us', ustk, cs ++ cstk, regs', frz')
  where (_, a, cs, es, us) = p !! n
        frz' = if ft then stk else frz
        es' = instAtoms frz' h regs es
        (us', regs') = foldl (instApp frz' h) ([], regs) us

step s = error $ "Missing pattern in step: " ++ show s

-- | Run `step` until the program is fully evaluated. Gives a lower bound on the
--   cycles required by the hardware implementation (equivalent except
--   heap-prefetching stalls)
run :: Prog
    -- ^ Input program
    -> (Integer, Int)
    -- ^ (Cycle count, Return value)
run prog = eval 0 initialState
  where
    initialState = ([FUN True 0 0], prog, [], [], [], replicate maxRegs (INT 0), [])
    eval n ([INT i], _, _, _, _, _, _) = (n, i)
    eval n s = eval (n+1) (step s)
    maxRegs = snatToNum (SNat @MaxRegs)

-- | Ensure that the semantics presented here give same result (in bounded
-- cycles) as the C emulator.
checkAll =
  do ok <- mapM check benchmarks
     return $ and ok
  where
    check fname =
      do p <- compileBenchmark fname
         (emuRet, emuCycles) <- runEmulator p
         let (cycles, ret) = run p
         putStrLn $ "Checking " ++ show fname ++ "... got " ++ show (cycles,ret)
           ++ " and expected " ++ show (emuCycles, emuRet)
         return $ cycles <= emuCycles && emuRet == fromIntegral ret
    benchmarks =
      [("./tests/benchmarks/adjoxo.fl"     )
      ,("./tests/benchmarks/braun.fl"      )
      ,("./tests/benchmarks/cichelli.fl"   )
      ,("./tests/benchmarks/clausify.fl"   )
      ,("./tests/benchmarks/countdown.fl"  )
      ,("./tests/benchmarks/fib.fl"        )
      ,("./tests/benchmarks/knuthbendix.fl")
      ,("./tests/benchmarks/mate.fl"       )
      ,("./tests/benchmarks/mss.fl"        )
      ,("./tests/benchmarks/ordlist.fl"    )
      ,("./tests/benchmarks/permsort.fl"   )
      ,("./tests/benchmarks/queens2.fl"    )
      ,("./tests/benchmarks/queens.fl"     )
      ,("./tests/benchmarks/sumpuz.fl"     )
      ,("./tests/benchmarks/taut.fl"       )
      ,("./tests/benchmarks/while.fl"      )
      ]

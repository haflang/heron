{-# LANGUAGE AllowAmbiguousTypes #-}

{-| Encoding F-lite templates (from `Flite.TemplateSyntax`) as native Heron
    `T.Template`s with fixed bit representations.
-}
module Heron.Encode
  (encProg
  ,dumpTemplates
  ,dumpTemplatesPy
  ) where

import qualified Clash.Prelude                  as C hiding (fromList)
import           Clash.Sized.Internal.BitVector (unsafeToNatural)
import           Clash.Sized.Vector             (fromList)
import           Data.List                      (findIndex, groupBy, isPrefixOf,
                                                 sortBy)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Flite.TemplateSyntax
import           Heron.Parameters
import qualified Heron.Template                 as T
import           Prelude

inRange :: forall a . (Bounded a, Integral a) => Int -> Bool
inRange n = let nmax = fromIntegral $ toInteger (maxBound :: a)
                nmin = fromIntegral $ toInteger (minBound :: a)
            in n >= nmin && n <= nmax

encOpcode :: String -> T.OpCode
encOpcode "(+)"  = T.OpAdd
encOpcode "(-)"  = T.OpSub
encOpcode "(==)" = T.OpEq
encOpcode "(/=)" = T.OpNeq
encOpcode "(<=)" = T.OpLeq
encOpcode "(!)"  = T.OpSeq
encOpcode f      = error $ "Encode.encOpcode: Invalid primitive op " ++ f

encAtom :: Atom -> T.Atom
encAtom (INT n)
  | inRange @T.PInt n
  = T.PrimInt $ fromIntegral n
encAtom (ARG s n)
  | inRange @T.ArgIndex n
  = T.Arg s $ fromIntegral n
encAtom (VAR s n)
  | inRange @T.HeapAddr n' &&
    n <  C.snatToNum (C.SNat @MaxAps) &&
    n >= negate (C.snatToNum $ C.SNat @MaxApSpan)
  = T.Ptr s $ fromIntegral n'
  where
    -- Negative relative offsets are all allowed for reference between split
    -- templates. If we alter encoding now, this resolves silently in the circuit
    n' = if n < 0 then n + 1 + fromIntegral (maxBound :: T.HeapAddr)
                  else n
encAtom (REG s n)
  | inRange @T.RegIndex n
  = T.Reg s $ fromIntegral n
encAtom (CON a t)
  | inRange @T.FnArity (a+1) &&
    inRange @T.Tag t
  = T.Con (fromIntegral a) (fromIntegral t)
encAtom (FUN i a f)
  | inRange @T.FnArity a &&
    inRange @T.TemplAddr f
  = T.Fun (fromIntegral a) (fromIntegral f) i
encAtom (PRI a op)
  | inRange @T.FnArity a
  = T.PrimOp (fromIntegral a) swap (encOpcode op')
  where
    swap = "swap:" `isPrefixOf` op
    op' = if swap then drop 5 op else op
encAtom a = error $ "Encode.encAtom: Invalid atom " ++ show a

toVec :: forall a n . C.KnownNat n => [a] -> C.Vec n (Maybe a)
toVec as
  | length as <= width
  = fromMaybe (error $ "Encode.toVect: List longer than Vect, " ++
                       show (length as) ++ " vs " ++ show width)
              (fromList $ as' ++ padding)
  | otherwise
  = error $ "Encode.toVect: input list shorter than Vect, " ++
               show (length as) ++ " vs " ++ show width
  where
    width = C.snatToNum (C.SNat :: C.SNat n)
    padding = replicate (width - length as) Nothing
    as' = map Just as

encAtoms :: forall n . C.KnownNat n => [Atom] -> C.Vec n (Maybe T.Atom)
encAtoms = toVec . map encAtom

encCaseTable :: LUT -> T.CaseTable T.Alt
encCaseTable (LOffset addr)
  | inRange @T.TemplAddr addr
  = T.CTOffset (fromIntegral addr)
  | otherwise
  = error "Encode.encCaseTable: offset address too large"
encCaseTable (LInline [altA])
  = T.CTInline (encAlt altA) (encAlt (0, CON 0 0))
encCaseTable (LInline [altA, altB])
  = T.CTInline (encAlt altA) (encAlt altB)
encCaseTable ct = error $ "Encode.encCaseTable: too many inline alternatives" ++ show ct

encAlt :: (Int, Atom) -> T.Alt
encAlt (p, INT val)
  | inRange @T.FnArity p &&
    inRange @T.ShortInt val
  = T.AInt (fromIntegral p) (fromIntegral val)
encAlt (p, ARG _ idx)
  | inRange @T.FnArity p &&
    inRange @T.ArgIndex idx
  = T.AArg (fromIntegral p) (fromIntegral idx)
encAlt (p, CON arity tag)
  | inRange @T.FnArity p &&
    inRange @T.FnArity arity &&
    inRange @T.ShortTag tag
  = T.ACon (fromIntegral p) (fromIntegral arity) (fromIntegral tag)
encAlt (_, FUN _ _ addr)
  | inRange @T.TemplAddr addr
  = T.AFun (fromIntegral addr)
encAlt a = error $ "Encode.encAlt: Invalid alt " ++ show a

encApp :: forall n m . (C.KnownNat n, C.KnownNat m) => App -> T.Node n m
encApp (APP isNF as)
  | length as <= maxWidth
  = T.App isNF
          (fromIntegral $ length as)
          (encAtoms as)
  where
    maxWidth = C.snatToNum (C.SNat :: C.SNat n)
encApp (CASE ct as)
  | length as <= maxWidth
  = T.Case (encCaseTable ct)
           (fromIntegral $ length as)
           (encAtoms as)
  where
    maxWidth = C.snatToNum (C.SNat :: C.SNat m)
encApp (PRIM reg as)
  | 3 == length as &&
    inRange @T.RegIndex reg
  = T.Prim (fromIntegral reg)
           (fromIntegral $ length as)
           (encAtoms as)
encApp app = error $ "Encode.encApp: Invalid app " ++ show app

encSpineApp :: (C.KnownNat nCase, C.KnownNat nApp)
            => [LUT] -> [Atom] -> T.Node nApp nCase
encSpineApp alts spineAp
  = case listToMaybe alts of
      Just alt -> T.Case (encCaseTable alt)
                         (fromIntegral $ length spineAp)
                         (encAtoms spineAp)
      Nothing  -> T.App  False
                         (fromIntegral $ length spineAp)
                         (encAtoms spineAp)

encTemplate :: Template -> T.Template
encTemplate (_, arity, alts, spineAp, heapAps)
  | inRange @T.PushOffset pushOffset               &&
    all (inRange @T.NodeArity . appLen) heapAps    &&
    length alts <= 1                               &&
    inRange @(T.Len MaxPush) (length spineAp   ) &&
    inRange @(T.Len MaxAps) (length heapAps)
  = T.Template (fromIntegral pushOffset)
               (encSpineApp alts spineAp)
               (toVec $ map encApp heapAps)
  where pushOffset = length spineAp - 1 - arity

encTemplate t
  = error $ "Encode.encTemplate: Invalid template " ++ show t

-- | Check that no split templates exceed `MaxFnAps` applications
checkMaxAps :: [Template] -> Bool
checkMaxAps templs = all ok fns
  where
    name (n, _, _, _, _) = n
    aps  (_, _, _, _, a) = a
    fns = groupBy (\a b -> name a == name b) $
          sortBy  (\a b -> compare (name a) (name b)) templs
    ok fn
      | sum (map (length . aps) fn) <= maxFnAps
      = True
      | otherwise
      = error $ "Encode.checkMaxAps: `MaxFnAps` violated by " ++ show (name $ head fn)
    maxFnAps = C.snatToNum (C.SNat @MaxFnAps)

-- | Encode an F-lite program as Heron `T.Template`s, with Clash bit
-- representations.
encProg :: [Template]
        -- ^ F-lite program
        -> (T.TemplAddr, [T.Template])
        -- ^ (Address of main, Heron program)
encProg templs
  | not (checkMaxAps templs)
  = error "Encode.encProg: Single function exceeds MaxFnAps"
  | otherwise
  = let templs' = map encTemplate templs
        mainAddr = fromMaybe (error "Encode.encProgram: No main template in program")
                             (findIndex (\(n,_,_,_,_) -> n=="main") templs)
    in (fromIntegral mainAddr, templs')

-- | Dump a set of Heron templates as bit vectors for interoperability.
-- Represented as a text file of binary strings with one template per line. (A
-- sane person would make this an actual binary file with some metadata about
-- the template width...)
dumpTemplates :: [T.Template] -> IO [String]
dumpTemplates prog = do
    let bits = map go prog
    return bits
  where
    -- Filter out leading '0b' and delimiting '_' characters
    go = filter (not . flip elem ['b','_']) . tail . show .
         repack . unsafeToNatural . C.pack
    repack :: Integral a => a -> C.BitVector (C.BitSize T.Template)
    repack = fromIntegral

-- | Dump a set of Heron templates as bit vectors for interoperability.
-- Represented a python list of binary strings.
dumpTemplatesPy :: [T.Template] -> IO [C.BitVector (C.BitSize T.Template)]
dumpTemplatesPy prog = do
    let bits = map (repack . unsafeToNatural . C.pack) prog
    return bits
  where
    repack :: Integral a => a -> C.BitVector (C.BitSize T.Template)
    repack = fromIntegral

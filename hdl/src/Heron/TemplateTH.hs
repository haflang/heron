module Heron.TemplateTH where

import Clash.Annotations.BitRepresentation.Deriving
import Language.Haskell.TH as TH
import Clash.Prelude (shiftL, shiftR, testBit, Bits)
import Prelude

deriveTyConDefaultAnnotations:: Q TH.Type -> [(Integer,Integer)] -> Q [Dec]
deriveTyConDefaultAnnotations tycon nats
  = do tycon' <- tycon
       let ty (a,b) = pure $ AppT (AppT tycon' (LitT (NumTyLit a))) (LitT (NumTyLit b))
       ans <- traverse (\n -> deriveDefaultAnnotation (ty n)) nats
       pure $ concat ans

deriveTyConBitPacks :: Q TH.Type -> [(Integer,Integer)] -> Q [Dec]
deriveTyConBitPacks tycon nats
  = do tycon' <- tycon
       let ty (a,b) = pure $ AppT (AppT tycon' (LitT (NumTyLit a))) (LitT (NumTyLit b))
       bps <- traverse (\n -> deriveBitPack           (ty n)) nats
       pure $ concat bps

-- | Generate bitmask from a given bit, with a certain size
bitmask
  :: Int
  -- ^ Bitmask starts at bit /n/
  -> Int
  -- ^ Bitmask has size /m/
  -> Integer
bitmask _start 0    = 0
bitmask start  size
  | start < 0        = error $ "Start cannot be <0. Was: " ++ show start
  | size < 0         = error $ "Size cannot be <0. Was: " ++ show size
  | start + 1 < size = error $ "Start + 1 (" ++ show start ++ " - 1) cannot be smaller than size (" ++ show size ++  ")."
  | otherwise        = shiftL (2 ^(toInteger size) - 1) (start - (size - 1))

fieldmasks :: [Int] -> [Integer]
fieldmasks = snd . go 0
  where go p [] = (p, [])
        go p (x:xs) = let (p', xs') = go p xs
                      in (p'+x, bitmask (p'+x-1) x : xs')

-- Promote an integral to a type-level Nat (a _quite_ stupid implementation)
promoteIntTH :: (Integral n, Bits n) => n -> ExpQ
promoteIntTH n
  | n<0         = error "Cannot promote negative int to SNat, dummy."
  | n==0        = [|d0|]
  | testBit n 0 = [| addSNat d1 (mulSNat d2 $(promoteIntTH $ shiftR n 1)) |]
  | otherwise   = [|             mulSNat d2 $(promoteIntTH $ shiftR n 1)  |]

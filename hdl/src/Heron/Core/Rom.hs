{-| ROM utilities -}
module Heron.Core.Rom
  ( -- * Generation
    newRom
  , -- * ROM Types
    RomAddr
  , RomIn
  , RomOut
  , Rom
  ) where

import Clash.Prelude

type RomAddr d = Index d

type RomIn d
  = RomAddr d

type RomOut a
  = a

type Rom dom a d = Signal dom (RomIn d) -> Signal dom (RomOut a)

-- | Generate a new ROM
newRom
  :: forall d a dom .
     ( KnownNat d
     , NFDataX  a
     , BitPack  a
     , HiddenClockResetEnable dom
     , 1 <= d
     )
  => FilePath
  -- ^ Memory contents
  -> Rom dom a d
  -- ^ ROM circuit
newRom file addr = outA
  where
    addr' :: Signal dom (Unsigned (CLog 2 d))
    addr'  = bitCoerce <$> addr
    outRaw = romFile (SNat @d) file addr'
    outA   = unpack <$> outRaw

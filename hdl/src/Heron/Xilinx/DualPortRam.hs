{-|

An unsafe primitive for Xilinx UltraRam/BlockRam.

This is a true dual-port RAM with some extra restrictions:
  + Both ports share a clock and a global enable
  + Only 'NO_EFFECT' mode is supported
  + Memory is uninitialised on restart

We reuse Clash's 'trueDualPortRam' simulation code, while only providing our own
verilog template for UltraRam inference. The simulation actually uses the
'READ_FIRST' mode. This isn't an issue for the current Heron implementation, but
can cause a mismatch between simulation behaviour and sythesised behaviour. Here
be dragons.

-}
{-# LANGUAGE QuasiQuotes, MagicHash #-}
module Heron.Xilinx.DualPortRam
  ( RamArch(..)
  , dpRamE
  , dpRam
  , deepUltraRam
  ) where

import Clash.Annotations.Primitive
import qualified Clash.Explicit.BlockRam as E
import Clash.Prelude
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)
import Data.Maybe (fromMaybe)

{-# ANN dpRamE# (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
    name: Heron.Xilinx.DualPortRam.dpRamE#
    kind: Declaration
    type: |-
      dpRamE# ::
        forall nAddrs dom a .
        ( KnownNat nAddrs              ~ARG[0]
        , KnownDomain dom              ~ARG[1]
        , NFDataX a                    ~ARG[2]
        )
        => String                      ~ARG[3]
        -> Clock dom                   ~ARG[4]
        -> Signal dom Bool             ~ARG[5]
        -> Signal dom Bool             ~ARG[6]
        -> Signal dom (Index nAddrs)   ~ARG[7]
        -> Signal dom a                ~ARG[8]
        -> Signal dom Bool             ~ARG[9]
        -> Signal dom (Index nAddrs)   ~ARG[10]
        -> Signal dom a                ~ARG[11]
        -> (Signal dom a, Signal dom a)
    template: |-
      // dpRamE# begin
      (* ram_style = ~ARG[3] *) reg [~SIZE[~TYP[8]]-1:0] ~GENSYM[mem][0] [~LIT[0]-1:0];
      reg ~SIGD[~GENSYM[data_slow][1]][8];
      reg ~SIGD[~GENSYM[data_fast][2]][11];
      reg ~SIGD[~GENSYM[data_slow_reg][3]][8];
      reg ~SIGD[~GENSYM[data_fast_reg][4]][11];
      // Port A
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              if(~ARG[6])
                  ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI] <= ~ARG[8];
              else
                  ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI];
          end
      end
      // Port B
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              if(~ARG[9])
                  ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
              else
                  ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
          end
      end
      // Output regs
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              ~SYM[3] <= ~SYM[1];
          end
      end
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              ~SYM[4] <= ~SYM[2];
          end
      end
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end dpRamE#
|]) #-}

dpRamE# ::
  forall nAddrs dom a .
  ( KnownNat nAddrs
  , KnownDomain dom
  , NFDataX a
  )
  => String
  -- ^ Ram type
  -> Clock dom
  -> Signal dom Bool
  -- ^ Global clock and enable
  -> Signal dom Bool
  -> Signal dom (Index nAddrs)
  -> Signal dom a
  -- ^ RAM operation for port A
  -> Signal dom Bool
  -> Signal dom (Index nAddrs)
  -> Signal dom a
  -- ^ RAM operation for port B
  -> (Signal dom a, Signal dom a)
  -- ^ Outputs data on /next/ cycle. When writing, the data written
  -- will be echoed. When reading, the read data is returned.
dpRamE# !_ clk en weA addrA datA weB addrB datB =
  -- unbundle . (\rest -> (errorX "Uninitialised UltraRam data") :- rest) . bundle $
  E.trueDualPortBlockRam# clk en weA addrA datA clk en weB addrB datB
{-# NOINLINE dpRamE# #-}

-- | A Xilinx dual-port memory primitive with explicit clock & enable
dpRamE ::
  forall nAddrs dom a .
  ( KnownNat nAddrs
  , KnownDomain dom
  , NFDataX a
  )
  => RamArch
  -- ^ Memory primitive configuration
  -> Clock dom
  -- ^ Global clock
  -> Enable dom
  -- ^ Global enable
  -> Signal dom (RamOp nAddrs a)
  -- ^ RAM operation for port A
  -> Signal dom (RamOp nAddrs a)
  -- ^ RAM operation for port B
  -> (Signal dom a, Signal dom a)
  -- ^ Data outputs. When reading, the read data is returned. When writing, the
  -- previous data is latched.
dpRamE arch clk en opA opB =
  dpRamE# (show arch) clk (fromEnable en)
    (isWr <$> opA) (getAddr <$> opA) (getData <$> opA)
    (isWr <$> opB) (getAddr <$> opB) (getData <$> opB)
  where
    isWr (RamWrite _ _) = True
    isWr _              = False

    getAddr (RamWrite a _) = a
    getAddr (RamRead  a  ) = a
    getAddr RamNoOp        = undefined

    getData (RamWrite _ d) = d
    getData (RamRead  _  ) = undefined
    getData RamNoOp        = undefined

-- | A Xilinx dual-port memory primitive with hidden clock, reset, and enable
dpRam ::
  forall nAddrs dom1 a .
  ( KnownNat nAddrs
  , HiddenClockResetEnable dom1
  , NFDataX a
  )
  => RamArch
  -- ^ Infer an ultra ram or a block ram
  -> Signal dom1 (RamOp nAddrs a)
  -- ^ RAM operation for port A
  -> Signal dom1 (RamOp nAddrs a)
  -- ^ RAM operation for port B
  -> (Signal dom1 a, Signal dom1 a)
  -- ^ Data outputs. When reading, the read data is returned. When writing, the
  -- previous data is latched.
dpRam arch = dpRamE arch (hasClock @dom1) (hasEnable @dom1)

-- | Ram primitive configuration
data RamArch
  = UltraRam -- ^ UltraRAM based
  | BlockRam -- ^ BlockRAM based (with constraints equivalent to UltraRAM)
  | DistRam  -- ^ Distributed memory

instance Show RamArch where
  show UltraRam = "ultra"
  show BlockRam = "block"
  show DistRam  = "distributed"

getAddr :: KnownNat n => RamOp n a -> Maybe (Index n)
getAddr (RamWrite addr _) = Just addr
getAddr (RamRead  addr  ) = Just addr
getAddr _ = Nothing

getSelIndex :: forall n
             . (KnownNat n, 1<=n, 1<= n `Div` 4096)
            => Index n -> Index (n `Div` 4096)
getSelIndex addr = unpack .  resize $ pack addr .&. pack mask
  where
    mask :: Index n
          = maxBound

shortRamOp :: forall n a
            . (KnownNat n, 1<=n, 1<= n `Div` 4096)
           => RamOp n a -> RamOp 4096 a
shortRamOp op = go op
  where
    go RamNoOp           = RamNoOp
    go (RamRead addr)    = RamRead  (short addr)
    go (RamWrite addr x) = RamWrite (short addr) x
    short :: Index n -> Index 4096
    short addr = unpack . resize $
                 shiftR (pack addr) (snatToNum $ SNat @(CLog 2 (n `Div` 4096)))

-- | An UltraRAM-based memory with external cascade logic. Constructs a parallel
--   set of \( n\times4K \) UlraRam blocks, where \( n \) is inferred by the
--   desired memory depth.
--
--   For particularly deep memories, this usually gives better timing at the
--   cost of extra LUTs. Something equivalent may be possible with
--   'CASCADE_HEIGHT' attributes in generated HDL.
deepUltraRam ::
  forall nAddrs dom1 a .
  ( KnownNat nAddrs
  , HiddenClockResetEnable dom1
  , NFDataX a
  , 1 <= nAddrs `Div` 4096
  , 1 <= nAddrs
  )
  => Signal dom1 (RamOp nAddrs a)
  -- ^ RAM operation for port A
  -> Signal dom1 (RamOp nAddrs a)
  -- ^ RAM operation for port B
  -> (Signal dom1 a, Signal dom1 a)
  -- ^ Data outputs. When reading, the read data is returned. When writing, the
  -- previous data is latched.
deepUltraRam a b = dataOut
  where
    nRams   = SNat @(nAddrs `Div` 4096)
    logRams = clogBaseSNat d2 nRams
    selA = (fromMaybe 0 . fmap getSelIndex . getAddr) <$> a
    selB = (fromMaybe 0 . fmap getSelIndex . getAddr) <$> b
    selA' = delay 0 selA
    selB' = delay 0 selB
    shortA = shortRamOp <$> a
    shortB = shortRamOp <$> b
    aCtrls = unbundle $ (\d i -> replace i d $ replicate nRams $ RamNoOp) <$> shortA <*> selA
    bCtrls = unbundle $ (\d i -> replace i d $ replicate nRams $ RamNoOp) <$> shortB <*> selB
    outs   = bundle $ zipWith (\x y -> bundle $ dpRam UltraRam x y) aCtrls bCtrls
    dataOut = ( (\os i -> fst (os !! i)) <$> outs <*> selA'
              , (\os i -> snd (os !! i)) <$> outs <*> selB')

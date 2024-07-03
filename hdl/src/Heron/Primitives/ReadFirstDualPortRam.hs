{-|

An unsafe primitive for Xilinx UltraRam/BlockRam.

This is a true dual-port RAM with some extra restrictions:
  + Both ports share a clock and a global enable
  + Only @READ_FIRST@ mode is supported
  + Memory is uninitialised on restart

-}
{-# LANGUAGE MagicHash   #-}
{-# LANGUAGE QuasiQuotes #-}
module Heron.Primitives.ReadFirstDualPortRam
  ( RamArch(..)
  , dpRamE
  , dpRam
  ) where

import           Clash.Annotations.Primitive
import           Clash.Prelude
import           Clash.XException.MaybeX            (MaybeX (..), andX)
import           Data.Sequence                      (Seq, update)
import           Data.String.Interpolate            (i)
import           Data.String.Interpolate.Util       (unindent)
import           GHC.Stack                          (HasCallStack)

import           Heron.Primitives.ReadFirstTDPModel

{-# ANN dpRamE# (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
    name: Heron.Primitives.ReadFirstDualPortRam.dpRamE#
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
              ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI];
              if(~ARG[6])
                  ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI] <= ~ARG[8];
          end
      end
      // Port B
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
              if(~ARG[9])
                  ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
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
dpRamE# !_ clk en weA addrA datA =
  trueDualPortBlockRam# clk en weA addrA datA clk en
{-# NOINLINE dpRamE# #-}
{-# ANN dpRamE# hasBlackBox #-}

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

-- | Primitive for 'trueDualPortBlockRam'
--
trueDualPortBlockRam# ::
  forall nAddrs domA domB a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domA
  , KnownDomain domB
  , NFDataX a
  ) =>

  Clock domA ->
  -- | Enable
  Signal domA Bool ->
  -- | Write enable
  Signal domA Bool ->
  -- | Address
  Signal domA (Index nAddrs) ->
  -- | Write data
  Signal domA a ->

  Clock domB ->
  -- | Enable
  Signal domB Bool ->
  -- | Write enable
  Signal domB Bool ->
  -- | Address
  Signal domB (Index nAddrs) ->
  -- | Write data
  Signal domB a ->

  (Signal domA a, Signal domB a)
trueDualPortBlockRam# clkA enA weA addrA datA clkB enB weB addrB =
  tdpbramModel
    TdpbramModelConfig
      { tdpIsActiveWriteEnable = id
      , tdpMergeWriteEnable = andX
      , tdpUpdateRam = updateRam }
    clkA enA addrA weA datA
    clkB enB addrB weB
 where
  updateRam :: Int -> MaybeX Bool -> a -> Seq a -> Seq a
  updateRam addr writeEnable dat mem =
    case writeEnable of
      IsDefined False -> mem
      IsDefined True -> update addr dat mem
      IsX msg -> update addr dat $ deepErrorX $
          "Write enable unknown; position" <> show addr <>
        "\nWrite enable error message: " <> msg

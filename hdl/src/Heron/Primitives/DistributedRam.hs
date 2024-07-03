{-|

A primitive for a single-port distributed LUT RAM on Xilinx devices.

Our simulation code is a direct copy of Clash's `blockRam1`. The verilog output
is nearly equivalent but we force mapping to distributed RAM using the
@ram_style@ attribute. Ideally we would generalise Clash's existing memory
primitives, but this addition will do as a stop-gap.

-}
{-# LANGUAGE MagicHash   #-}
{-# LANGUAGE QuasiQuotes #-}
module Heron.Primitives.DistributedRam
  ( distRam1
  , distRam1E
  ) where

import           Clash.Annotations.Primitive
import qualified Clash.Explicit.BlockRam      as EB
import qualified Clash.Explicit.Prelude       as E
import           Clash.Prelude
import           Clash.Signal.Internal        (invertReset)
import           Data.Maybe                   (isJust)
import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           GHC.Stack                    (HasCallStack)

-- | A distributed LUTRAM that is initialized with the same value on all
-- memory positions. Clock, reset, and enable are all hidden.
distRam1
   :: forall n dom a r addr
   . ( HiddenClockResetEnable dom
     , NFDataX a
     , Enum addr
     , NFDataX addr
     , GHC.Stack.HasCallStack
     , 1 <= n )
  => ResetStrategy r
  -- ^ Whether to clear BRAM on asserted reset ('ClearOnReset') or
  -- not ('NoClearOnReset'). The reset needs to be asserted for at least /n/
  -- cycles to clear the BRAM.
  -> SNat n
  -- ^ Number of elements in BRAM
  -> a
  -- ^ Initial content of the BRAM (replicated /n/ times)
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
distRam1 = hideClockResetEnable distRam1E

-- | A distributed LUTARM that is initialized with the same value on all
-- memory positions. Clock, reset, and enable are all exposed.
distRam1E
   :: forall n dom a r addr
   . ( KnownDomain dom
     , GHC.Stack.HasCallStack
     , NFDataX a
     , Enum addr
     , NFDataX addr
     , 1 <= n )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Reset dom
  -- ^ 'Reset' line. This needs to be asserted for at least /n/ cycles in order
  -- for the BRAM to be reset to its initial state.
  -> Enable dom
  -- ^ 'Enable' line
  -> ResetStrategy r
  -- ^ Whether to clear BRAM on asserted reset ('ClearOnReset') or
  -- not ('NoClearOnReset'). The reset needs to be asserted for at least /n/
  -- cycles to clear the BRAM.
  -> SNat n
  -- ^ Number of elements in BRAM
  -> a
  -- ^ Initial content of the BRAM (replicated /n/ times)
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal dom a
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
distRam1E clk rst0 en rstStrategy n@SNat a rd0 mw0 =
  case rstStrategy of
    ClearOnReset ->
      -- Use reset infrastructure
      distRam1E# clk en n a rd1 we1 wa1 w1
    NoClearOnReset ->
      -- Ignore reset infrastructure, pass values unchanged
      distRam1E# clk en n a
        (fromEnum <$> rd0)
        we0
        (fromEnum <$> wa0)
        w0
 where
  rstBool = E.register clk rst0 en True (pure False)
  rstInv = invertReset rst0

  waCounter :: Signal dom (Index n)
  waCounter = E.register clk rstInv en 0 (satSucc SatBound <$> waCounter)

  wa0 = fst . fromJustX <$> mw0
  w0  = snd . fromJustX <$> mw0
  we0 = isJust <$> mw0

  rd1 = mux rstBool 0 (fromEnum <$> rd0)
  we1 = mux rstBool (pure True) we0
  wa1 = mux rstBool (fromInteger . toInteger <$> waCounter) (fromEnum <$> wa0)
  w1  = mux rstBool (pure a) w0

-- | blockRAM1 primitive
distRam1E#
  :: forall n dom a
   . ( KnownDomain dom
     , GHC.Stack.HasCallStack
     , NFDataX a )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> SNat n
  -- ^ Number of elements in BRAM
  -> a
  -- ^ Initial content of the BRAM (replicated /n/ times)
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom a
  -- ^ Value to write (at address @w@)
  -> Signal dom a
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
distRam1E# clk en n a =
  -- TODO: Generalize to single BRAM primitive taking an initialization function
  EB.blockRam# clk en (replicate n a)
{-# NOINLINE distRam1E# #-}

{-# ANN distRam1E# (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
    name: Heron.Primitives.DistributedRam.distRam1E#
    kind: Declaration
    outputUsage: NonBlocking
    type: |-
      distRam1E#
        :: ( KnownDomain dom        ARG[0]
           , HasCallStack  --       ARG[1]
           , Undefined a ) --       ARG[2]
        => Clock dom       -- clk,  ARG[3]
        -> Enable dom      -- en,   ARG[4]
        -> SNat n          -- len,  ARG[5]
        -> a               -- init, ARG[6]
        -> Signal dom Int  -- rd,   ARG[7]
        -> Signal dom Bool -- wren, ARG[8]
        -> Signal dom Int  -- wr,   ARG[9]
        -> Signal dom a    -- din,  ARG[10]
        -> Signal dom a
    template: |-
      // blockRam1 begin
      (* ram_style = "distributed" *) reg ~TYPO ~GENSYM[~RESULT_RAM][0] [0:~LIT[5]-1];
      integer ~GENSYM[i][1];
      initial begin
          for (~SYM[1]=0;~SYM[1]<~LIT[5];~SYM[1]=~SYM[1]+1) begin
              ~SYM[0][~SYM[1]] = ~CONST[6];
          end
      end

      ~IF ~ISACTIVEENABLE[4] ~THEN
      always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~RESULT_blockRam][5]~IF ~VIVADO ~THEN
        if (~ARG[4]) begin
          if (~ARG[8]) begin
            ~SYM[0][~ARG[9]] <= ~ARG[10];
          end
          ~RESULT <= ~SYM[0][~ARG[7]];
        end~ELSE
        if (~ARG[8] & ~ARG[4]) begin
          ~SYM[0][~ARG[9]] <= ~ARG[10];
        end
        if (~ARG[4]) begin
          ~RESULT <= ~SYM[0][~ARG[7]];
        end~FI
      end~ELSE
      always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[5]
        if (~ARG[8]) begin
          ~SYM[0][~ARG[9]] <= ~ARG[10];
        end
        ~RESULT <= ~SYM[0][~ARG[7]];
      end~FI
      // blockRam1 end
|]) #-}

{-# LANGUAGE QuasiQuotes #-}

{-| Clock division utilities for Xilinx UltraScale+ using matched `BUFGCE_DIV`
  primitives.

  Also provides a helper function for multipumping a circuit --- clocking
  a subcircuit at twice the base rate. Care must be taken to manually insert
  valid Multi-Cycle Path (MCP) constraints.
-}
module Heron.Xilinx.ClockDiv
  (-- * Clock Division
   DividedClks
  ,clockDiv
   -- * Multi-cycle path utilities
  ,multiPump
  ) where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench
import Clash.Annotations.Primitive
import Clash.Annotations.TH
import Unsafe.Coerce
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)

-- | Synonym constraint for two related, divided clocks. The `fast` domain is
--   `divN` times faster than the `slow` domain.
type DividedClks divN fast slow period edge reset init polarity
  = ( KnownDomain fast
    , KnownDomain slow
    , KnownConfiguration fast ('DomainConfiguration fast period          edge reset init polarity)
    , KnownConfiguration slow ('DomainConfiguration slow (divN * period) edge reset init polarity)
    )

-- | Synonym for `E.register` but it generates a special entity name in verilog
--   output. Useful for identification in constraints scripts --- we otherwise
--   need to differentiate between the true data path registers and the
--   register used to generate gated clock enable.
mcpRegIn :: (KnownDomain dom, NFDataX a)
       => Clock dom -> Reset dom -> Enable dom -> Signal dom a -> Signal dom a
mcpRegIn clk rst en a = regA
  where
    regA = E.register clk rst en undefined a
{-# NOINLINE mcpRegIn #-}

-- | Synonym for `E.register` but it generates a special entity name in verilog
--   output. Useful for identification in constraints scripts --- we otherwise
--   need to differentiate between the true data path registers and the
--   register used to generate gated clock enable.
mcpRegOut :: (KnownDomain dom, NFDataX a)
       => Clock dom -> Reset dom -> Enable dom -> Signal dom a -> Signal dom a
mcpRegOut clk rst en a = regA
  where
    regA = E.register clk rst en undefined a
{-# NOINLINE mcpRegOut #-}

{-| Multipump a subcircuit from the `domFast` domain with `mcpRegIn` and
    `mcpRegOut` . Apply MCP constraints to relax timing for interfaces to/from
    the multipumped circuit. For Vivado, this likely includes:
      1) Relaxing MCPs between `domSlow` and `domFast`
      2) Relaxing any combinatorial paths between the `mcpRegOut` and `mcpRegIn` registers
      3) Ensuring the `mcpRegIn`/`mcpRegOut` hierarchies are preserved during synthesis

    A dubious example:

    > # Preserve hierarchy
    > set mcp_reg_hiers [get_cells -hier -filter { ORIG_REF_NAME =~ *mcpReg* } ]
    > set_property keep_hierarchy yes $mcp_reg_hiers
    >
    > # MCPs for clock domain crossings
    > set fastclk [get_clocks clk]
    > set slowclk [get_clocks -of_objects [get_pins BUFGCE_DIV_inst_0/O]]
    >
    > set_multicycle_path 2 -setup -start -from $fastclk -to $slowclk
    > set_multicycle_path 1 -hold         -from $fastclk -to $slowclk
    >
    > set_multicycle_path 2 -setup        -from $slowclk -to $fastclk
    > set_multicycle_path 1 -hold  -end   -from $slowclk -to $fastclk
    >
    > # MCPs for loops between mcpRegOut and mcpRegIns
    > set mcp_out [get_pins  -of_objects [get_cells -hier *mcpRegOut*] -filter {REF_PIN_NAME =~ *result* }]
    > set mcp_out_nets [all_fanin -startpoints_only [get_nets $mcp_out]]
    >
    > set mcp_in [get_pins  -of_objects [get_cells -hier *mcpRegIn*] -filter {REF_PIN_NAME =~ *a1* }]
    > set mcp_in_nets [all_fanout -endpoints_only [get_nets $mcp_in]]
    >
    > set_multicycle_path 2 -setup      -from $mcp_out_nets -to $mcp_in_nets
    > set_multicycle_path 1 -hold  -end -from $mcp_out_nets -to $mcp_in_nets
-}
multiPump
  :: forall domFast domSlow periodIn edge reset init polarity a b
   . ( DividedClks 2 domFast domSlow periodIn edge reset init polarity
     , NFDataX a
     , NFDataX b )
  => Clock domFast  -- ^ Fast clock
  -> Reset domFast  -- ^ Fast reset
  -> Enable domFast -- ^ Fast clock enable
  -> Clock domSlow  -- ^ Slow clock
  -> (Clock domFast -> Reset domFast -> Enable domFast -> Signal domFast a -> Signal domFast b)
  -- ^ Our circuit in the fast domain. Takes 1 cycle to compute result
  -> Signal domSlow a -> Signal domSlow a -> (Signal domSlow b, Signal domSlow b)
  -- ^ Wrapped circuit exposed in `domSlow` domain as a dual-ported version.
multiPump clkFast rstFast enFast clkSlow circ a b = (c,d)
  where
    fastA = E.unsafeSynchronizer clkSlow clkFast a
    fastB = E.unsafeSynchronizer clkSlow clkFast b
    ce    = E.register clkFast rstFast enFast False ceBar
    ceBar = not <$> ce
    regA  = mcpRegIn clkFast rstFast (toEnable ce) fastA
    regB  = mcpRegIn clkFast rstFast (toEnable ce) fastB
    muxedIns = mux ce regA regB
    muxedOuts = circ clkFast rstFast enFast muxedIns
    muxedOuts' = E.register clkFast rstFast enFast undefined muxedOuts
    regC  = mcpRegOut clkFast rstFast (toEnable ce) muxedOuts'
    regD  = mcpRegOut clkFast rstFast (toEnable ce) muxedOuts
    c     = E.unsafeSynchronizer clkFast clkSlow regC
    d     = E.unsafeSynchronizer clkFast clkSlow regD
{-# NOINLINE multiPump #-}

-- | A clock divider circuit with matched phases for UltraScale+.
--   We take an input clock and split it into two matched paths of `BUFGCE_DIV`s.
--   The first is buffered version of the original, the second is the divided
--   clock (in phase with the first output).
clockDiv
  :: forall divN domIn domFast domSlow periodIn edge reset init polarity
   . ( KnownConfiguration domIn   ('DomainConfiguration domIn           periodIn  edge reset init polarity)
     , KnownConfiguration domFast ('DomainConfiguration domFast         periodIn  edge reset init polarity)
     , KnownConfiguration domSlow ('DomainConfiguration domSlow (divN * periodIn) edge reset init polarity) )
  => SNat divN
  -- ^ Divide factor
  -> Clock domIn
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset domIn
  -- ^ Reset for the divider
  -> Enable domIn
  -- ^ Enable for the divider
  -> (Clock domFast, Clock domSlow)
  -- ^ Output clocks
clockDiv !n clk !rst !en = ( unsafeCoerce clk, unsafeCoerce clk)
{-# NOINLINE clockDiv #-}
{-# ANN clockDiv hasBlackBox #-}

{-# ANN clockDiv (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
   kind: Declaration
   name: Heron.Xilinx.ClockDiv.clockDiv
   type: |-
     clockDiv
       :: ( KnownDomain domIn confIn       -- ARG[0]
          , KnownDomain domFast confFast   -- ARG[1]
          , KnownDomain domSlow confSlow ) -- ARG[2]
       => SNat divN                        -- ARG[3]
       -> Clock  pllIn                     -- ARG[4]
       -> Reset  pllIn                     -- ARG[5]
       -> Enable pllIn                     -- ARG[6]
       -> (Clock pllFast, Clock pllSlow)
   template: |-
     // clockDiv begin
     BUFGCE_DIV #(
       .BUFGCE_DIVIDE(1),
       .IS_CE_INVERTED(0),
       .IS_CLR_INVERTED(0),
       .IS_I_INVERTED(0)
     ) ~GENSYM[BUFGCE_DIV_inst][0] (
       .O(~RESULT[1]),
       .CE(~ARG[6]),
       .CLR(~ARG[5]),
       .I(~ARG[4])
     );
     BUFGCE_DIV #(
       .BUFGCE_DIVIDE(~LIT[3]),
       .IS_CE_INVERTED(0),
       .IS_CLR_INVERTED(0),
       .IS_I_INVERTED(0)
     ) ~GENSYM[BUFGCE_DIV_inst][1] (
       .O(~RESULT[0]),
       .CE(~ARG[6]),
       .CLR(~ARG[5]),
       .I(~ARG[4])
     );
     // clockDiv end

|]) #-}

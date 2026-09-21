-- |

module Heron.TraceFSM
  ( traceFSM
  ) where

import           Clash.Annotations.Primitive
import           Clash.Prelude
import           Clash.Signal.Internal        (Signal ((:-)))
import qualified Data.List                    as L
import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           RetroClash.Barbies
import           RetroClash.CPU

import           Barbies
import           Barbies.Bare
import           Control.Monad.State
import           Data.Typeable
import           Heron.Template               (CoreId)

{-# ANN traceCoreSignal (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
    name: Heron.TraceFSM.traceCoreSignal
    kind: Declaration
    type: |-
      traceCoreSignal ::
        forall dom a .
        ( BitPack a              ~ARG[0]
        , NFDataX a              ~ARG[1]
        , Typeable a             ~ARG[2]
        )
        => String                ~ARG[3]
        -> Signal dom CoreId     ~ARG[4]
        -> Signal dom a          ~ARG[5]
        -> Signal dom a
    template: |-
      ~ARG[5]
|]) #-}

traceCoreSignal
  :: (BitPack a, NFDataX a, Typeable a)
  => String -> Signal dom CoreId -> Signal dom a -> Signal dom a
traceCoreSignal suffix cid = traceSignal1 name
   where
     (coreId :- _) = cid
     name = L.concat ["Core_", show coreId, suffix]
{-# NOINLINE traceCoreSignal #-}
{-# ANN traceCoreSignal hasBlackBox #-}

traceFSM
    :: (BareB i, TraversableB (i Covered), DistributiveB (i Covered))
    => (NFDataX s, BitPack s, AutoReg s)
    => (NFDataX (Pure i), BitPack (Pure i))
    => (NFDataX (Pure o), BitPack (Pure o))
    => (BareB o, TraversableB (o Covered), ApplicativeB (o Covered), DistributiveB (o Covered))
    => (Typeable i, Typeable o, Typeable s)
    => (HiddenClockResetEnable dom)
    => String
    -> s
    -> (s -> Pure o)
    -> (Pure i -> CPUM s o ())
    -> Signal dom CoreId
    -> Signals dom i -> Signals dom o
traceFSM prefix initState defaultOutput step cid ins = traces `seqX` o
  where
    (s,o) = mealyFSM initState defaultOutput step ins
    traces = traceCoreSignal prefix cid $ bundle (bbundle ins, s, bbundle o)
{-# INLINE traceFSM #-}

mealyFSM
    :: (BareB i, TraversableB (i Covered))
    => (NFDataX s, AutoReg s)
    => (BareB o, ApplicativeB (o Covered), DistributiveB (o Covered))
    => (HiddenClockResetEnable dom)
    => s
    -> (s -> Pure o)
    -> (Pure i -> CPUM s o ())
    -> Signals dom i -> (Signal dom s, Signals dom o)
mealyFSM initState defaultOutput step ins =
  let (s,o) = fsmState (runCPU defaultOutput . step) initState $ bbundle ins
  in (s, bunbundle o)

fsmState
   :: (HiddenClockResetEnable dom, NFDataX s, AutoReg s)
   => (i -> State s o) -> s -> (Signal dom i -> (Signal dom s, Signal dom o))
fsmState f = mealy' step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

mealy'
  :: ( HiddenClockResetEnable dom
     , NFDataX s, AutoReg s )
  => (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Signal dom i -> (Signal dom s, Signal dom o))
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealy' f iS ins =
  let (s',o) = unbundle $ f <$> s <*> ins
      s      = autoReg iS s'
  in (s', o)

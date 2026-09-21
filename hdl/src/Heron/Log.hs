{-# LANGUAGE MagicHash #-}
module Heron.Log where

import           Clash.Prelude
import           Clash.Signal.Trace
import           Data.IORef         (readIORef)
import qualified Data.List          as L
import qualified Data.Map.Strict    as Map
import           Heron.VCD

type Trace = (TypeRepBS, Period, Width, [Value])
type TraceMap  = Map.Map String Trace

-- A backend for tracing internal signals.
-- A replacement for the VCD output --- we are free to format the streams however we want
dumpSignals
  :: forall s a dom . (TraceState s, NFDataX a)
  => (Int, Int)
  -- ^ (Offset, Cycles)
  -> Signal dom a
  -- ^ (One of) the output(s) the circuit containing the traces
  -> IO [s]
dumpSignals (offset, cycles) signal = do
  _ <- waitForTraces# traceMap# signal keys'
  m <- readIORef traceMap#
  let xs = L.map recreateBV . L.transpose $ L.map (vals m) keys'
  pure xs
  where
    keys' = keys @s
    vals m s = L.drop offset . L.take cycles . (\(_,_,_,vs)->vs) $ m Map.! s

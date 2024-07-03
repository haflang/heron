{-|
Copyright  :  (C) 2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Configurable model for true dual-port block RAM
-}

{-
This is a lightly modified version of the original Clash source.
We ignore conflicts and perform port A operations before port B.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE MagicHash    #-}

module Heron.Primitives.UltraRamModel
 (trueDualPortBlockRam#) where

import           Clash.Explicit.Prelude
import           Clash.Signal.Internal  (Signal ((:-)))
import           Data.Maybe             (fromMaybe)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Tuple             (swap)
import           GHC.Stack              (HasCallStack)

-- | Primitive of 'Heron.Primitives.trueDualPortBlockRam'.
trueDualPortBlockRam# ::
  forall nAddrs domA domB a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domA
  , KnownDomain domB
  , NFDataX a
  )
  => Clock domA
  -- ^ Clock for port A
  -> Signal domA Bool
  -- ^ Enable for port A
  -> Signal domA Bool
  -- ^ Write enable for port A
  -> Signal domA (Index nAddrs)
  -- ^ Address to read from or write to on port A
  -> Signal domA a
  -- ^ Data in for port A; ignored when /write enable/ is @False@

  -> Clock domB
  -- ^ Clock for port B
  -> Signal domB Bool
  -- ^ Enable for port B
  -> Signal domB Bool
  -- ^ Write enable for port B
  -> Signal domB (Index nAddrs)
  -- ^ Address to read from or write to on port B
  -> Signal domB a
  -- ^ Data in for port B; ignored when /write enable/ is @False@

  -> (Signal domA a, Signal domB a)
  -- ^ Outputs data on /next/ cycle. If write enable is @True@, the data written
  -- will be echoed. If write enable is @False@, the read data is returned. If
  -- port enable is @False@, it is /undefined/.
trueDualPortBlockRam# clkA enA weA addrA datA clkB enB weB addrB datB
  | snatToNum @Int (clockPeriod @domA) < snatToNum @Int (clockPeriod @domB)
  = swap (trueDualPortBlockRamModel labelB clkB enB weB addrB datB labelA clkA enA weA addrA datA)
  | otherwise
  =       trueDualPortBlockRamModel labelA clkA enA weA addrA datA labelB clkB enB weB addrB datB
 where
  labelA = "Port A"
  labelB = "Port B"


-- | Haskell model for the primitive 'trueDualPortBlockRam#'.
--
-- Warning: this model only works if @domFast@'s clock is faster (or equal to)
-- @domSlow@'s clock.
trueDualPortBlockRamModel ::
  forall nAddrs domFast domSlow a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain domSlow
  , KnownDomain domFast
  , NFDataX a
  ) =>

  String ->
  Clock domSlow ->
  Signal domSlow Bool ->
  Signal domSlow Bool ->
  Signal domSlow (Index nAddrs) ->
  Signal domSlow a ->

  String ->
  Clock domFast ->
  Signal domFast Bool ->
  Signal domFast Bool ->
  Signal domFast (Index nAddrs) ->
  Signal domFast a ->

  (Signal domSlow a, Signal domFast a)
trueDualPortBlockRamModel labelA !_clkA enA weA addrA datA labelB !_clkB enB weB addrB datB =
  ( startA :- outA
  , startB :- outB )
 where
  (outA, outB) =
    go
      (Seq.fromFunction (natToNum @nAddrs) initElement)
      0 -- ensure 'go' hits fast clock first for 1 cycle, then execute slow
         -- clock for 1 cycle, followed by the regular cadence of 'ceil(tA / tB)'
         -- cycles for the fast clock followed by 1 cycle of the slow clock
      (bundle (enA, weA, fromIntegral <$> addrA, datA))
      (bundle (enB, weB, fromIntegral <$> addrB, datB))
      startA startB

  tA = snatToNum @Int (clockPeriod @domSlow)
  tB = snatToNum @Int (clockPeriod @domFast)

  startA = deepErrorX $ "trueDualPortBlockRam: " <> labelA <> ": First value undefined"
  startB = deepErrorX $ "trueDualPortBlockRam: " <> labelB <> ": First value undefined"

  initElement :: Int -> a
  initElement n =
    deepErrorX ("Unknown initial element; position " <> show n)

  unknownEnableAndAddr :: String -> String -> Int -> a
  unknownEnableAndAddr enaMsg addrMsg n =
    deepErrorX ("Write enable and data unknown; position " <> show n <>
                "\nWrite enable error message: " <> enaMsg <>
                "\nAddress error message: " <> addrMsg)

  unknownAddr :: String -> Int -> a
  unknownAddr msg n =
    deepErrorX ("Write enabled, but address unknown; position " <> show n <>
                "\nAddress error message: " <> msg)

  writeRam :: Bool -> Int -> a -> Seq a -> (Maybe a, Seq a)
  writeRam enable addr dat mem
    | Left enaMsg <- enableUndefined
    , Left addrMsg <- addrUndefined
    = let msg = "Unknown enable and address" <>
                "\nWrite enable error message: " <> enaMsg <>
                "\nAddress error message: " <> addrMsg
       in ( Just (deepErrorX msg)
          , Seq.fromFunction (natToNum @nAddrs)
                             (unknownEnableAndAddr enaMsg addrMsg) )
    | Left enaMsg <- enableUndefined
    = let msg = "Write enable unknown; position" <> show addr <>
                "\nWrite enable error message: " <> enaMsg
       in writeRam True addr (deepErrorX msg) mem
    | enable
    , Left addrMsg <- addrUndefined
    = ( Just (deepErrorX "Unknown address")
      , Seq.fromFunction (natToNum @nAddrs) (unknownAddr addrMsg) )
    | enable
    = (Just dat, Seq.update addr dat mem)
    | otherwise
    = (Nothing, mem)
   where
    enableUndefined = isX enable
    addrUndefined = isX addr

  go ::
    Seq a ->
    Int ->
    Signal domSlow (Bool, Bool, Int, a) ->
    Signal domFast (Bool, Bool, Int, a) ->
    a -> a ->
    (Signal domSlow a, Signal domFast a)
  go ram0 relativeTime as0 bs0 =
    case compare relativeTime 0 of
      LT -> goSlow
      EQ -> goBoth
      GT -> goFast
   where
    (enA_, weA_, addrA_, datA_) :- as1 = as0
    (enB_, weB_, addrB_, datB_) :- bs1 = bs0

    goBoth prevA prevB = outA2 `seqX` outB2 `seqX` (outA2 :- as2, outB2 :- bs2)
     where

      (datA1_,datB1_) = (datA_,datB_)

      (wroteA,ram1) = writeRam weA_ addrA_ datA1_ ram0
      (wroteB,ram2) = writeRam weB_ addrB_ datB1_ ram1

      outA1 = fromMaybe (ram0 `Seq.index` addrA_) wroteA

      outB1 = fromMaybe (ram0 `Seq.index` addrB_) wroteB

      outA2 = if enA_ then outA1 else prevA
      outB2 = if enB_ then outB1 else prevB
      (as2,bs2) = go ram2 (relativeTime - tB + tA) as1 bs1 outA2 outB2

    -- 1 iteration here, as this is the slow clock.
    goSlow _ prevB | enA_ = out0 `seqX` (out0 :- as2, bs2)
     where
      (wrote, !ram1) = writeRam weA_ addrA_ datA_ ram0
      out0 = fromMaybe (ram1 `Seq.index` addrA_) wrote
      (as2, bs2) = go ram1 (relativeTime + tA) as1 bs0 out0 prevB

    goSlow prevA prevB = (prevA :- as2, bs2)
      where
        (as2,bs2) = go ram0 (relativeTime + tA) as1 bs0 prevA prevB

    -- 1 or more iterations here, as this is the fast clock. First iteration
    -- happens here.
    goFast prevA _ | enB_ = out0 `seqX` (as2, out0 :- bs2)
     where
      (wrote, !ram1) = writeRam weB_ addrB_ datB_ ram0
      out0 = fromMaybe (ram1 `Seq.index` addrB_) wrote
      (as2, bs2) = go ram1 (relativeTime - tB) as0 bs1 prevA out0

    goFast prevA prevB = (as2, prevB :- bs2)
     where
       (as2,bs2) = go ram0 (relativeTime - tB) as0 bs1 prevA prevB

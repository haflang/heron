{-

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE QuasiQuotes  #-}

module Heron.Primitives.UltraRamModel
 (ultraRam#, topEntity) where

import           Clash.Annotations.Primitive
import           Clash.Annotations.TH
import           Clash.Explicit.Prelude
import           Clash.Signal.Internal        (Signal ((:-)))
import           Data.Either                  (isRight)
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           GHC.Stack                    (HasCallStack)

{-# ANN ultraRam# (InlineYamlPrimitive [Verilog] $ unindent [i|
 BlackBox:
    name: Heron.Primitives.UltraRamModel.ultraRam#
    kind: Declaration
    type: |-
      ultraRamE# ::
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
      // ultraRamE# begin
      (* ram_style = ~ARG[3] *) reg [~SIZE[~TYP[8]]-1:0] ~GENSYM[mem][0] [~LIT[0]-1:0];
      wire ~SIGD[~GENSYM[outAr][1]][8];
      wire ~SIGD[~GENSYM[outBr][2]][11];
      reg ~SIGD[~GENSYM[fwd][3]][5];
      reg ~SIGD[~GENSYM[fwdData][4]][8];
      reg ~SIGD[~GENSYM[prevA][5]][8];
      reg ~SIGD[~GENSYM[prevB][6]][11];
      reg ~SIGD[~GENSYM[rdA][7]][8];
      reg ~SIGD[~GENSYM[rdB][8]][11];
      reg ~SIGD[~GENSYM[latchA][9]][8];
      reg ~SIGD[~GENSYM[latchB][10]][11];

      // Port A
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              if(~ARG[6])
                  ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI] <= ~ARG[8];
              else
                  ~SYM[7] <= ~SYM[0][~IF~SIZE[~TYP[7]]~THEN~ARG[7]~ELSE0~FI];
          end
      end
      // Port B
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
          if(~ARG[5]) begin
              if(~ARG[9])
                  ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
              else
                  ~SYM[8] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
          end
      end

      // Additional latching and forwarding for correct simulation
      always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[4]) begin
            ~SYM[9]  <= !~ARG[6] && (^~ARG[7]  !== 1'bX);
            ~SYM[10] <= !~ARG[9] && (^~ARG[10] !== 1'bX);
            ~SYM[5] <= ~SYM[1];
            ~SYM[6] <= ~SYM[2];
            ~SYM[3] <= ~ARG[6] && !~ARG[9] && ~ARG[7] === ~ARG[10];
            ~SYM[4] <= ~ARG[8];
      end

      assign ~SYM[1] = ~SYM[9] ? ~SYM[7] : ~SYM[5];
      assign ~SYM[2] = ~SYM[3] ? ~SYM[4] : (~SYM[10] ? ~SYM[8] : ~SYM[6]);
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end ultraRamE#
|]) #-}

-- | Primitive of 'Heron.Primitives.ultraRam'.
ultraRam# ::
  forall nAddrs dom a .
  -- ( HasCallStack -- Removed so we don't have to rewrite URAM verilog template
  ( KnownNat nAddrs
  , KnownDomain dom
  , NFDataX a
  )
  => String
  -> Clock dom
  -- ^ Clock
  -> Signal dom Bool
  -- ^ Enable

  -> Signal dom Bool
  -- ^ Write enable for port A
  -> Signal dom (Index nAddrs)
  -- ^ Address to read from or write to on port A
  -> Signal dom a
  -- ^ Data in for port A; ignored when /write enable/ is @False@

  -> Signal dom Bool
  -- ^ Write enable for port B
  -> Signal dom (Index nAddrs)
  -- ^ Address to read from or write to on port B
  -> Signal dom a
  -- ^ Data in for port B; ignored when /write enable/ is @False@

  -> (Signal dom a, Signal dom a)
  -- ^ Outputs data on /next/ cycle. If write enable is @True@, the data written
  -- will be echoed. If write enable is @False@, the read data is returned. If
  -- port enable is @False@, it is /undefined/.
ultraRam# !_ clk en weA addrA datA
  = ultraRamModel clk en labelA weA addrA datA labelB
 where
  labelA = "Port A"
  labelB = "Port B"
{-# NOINLINE ultraRam# #-}


-- | Haskell model for the primitive 'ultraRam#'.
ultraRamModel ::
  forall nAddrs dom a .
  ( HasCallStack
  , KnownNat nAddrs
  , KnownDomain dom
  , NFDataX a
  , dom ~ dom
  ) =>
  Clock dom ->
  Signal dom Bool ->

  String ->
  Signal dom Bool ->
  Signal dom (Index nAddrs) ->
  Signal dom a ->

  String ->
  Signal dom Bool ->
  Signal dom (Index nAddrs) ->
  Signal dom a ->

  (Signal dom a, Signal dom a)
ultraRamModel !_clk en labelA weA addrA datA labelB weB addrB datB =
  ( startA :- outA
  , startB :- outB )
 where
  (outA, outB) =
    go
      (Seq.fromFunction (natToNum @nAddrs) initElement)
      (bundle (en, weA, fromIntegral <$> addrA, datA))
      (bundle (en, weB, fromIntegral <$> addrB, datB))
      startA startB

  startA = errorX $ unwords ["ultraRam:", labelA, ": First value undefined"]
  startB = errorX $ unwords ["ultraRam:", labelB, ": First value undefined"]

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
    Signal domSlow (Bool, Bool, Int, a) ->
    Signal domFast (Bool, Bool, Int, a) ->
    a -> a ->
    (Signal domSlow a, Signal domFast a)
  go ram0 as0 bs0 prevA prevB =
    outA2 `seqX` outB2 `seqX` (outA2 :- as2, outB2 :- bs2)
   where
    (enA_, weA_, addrA_, datA_) :- as1 = as0
    (enB_, weB_, addrB_, datB_) :- bs1 = bs0

    (datA1_,datB1_) = (datA_,datB_)

    (wroteA,ram1) = writeRam weA_ addrA_ datA1_ ram0
    (wroteB,ram2) = writeRam weB_ addrB_ datB1_ ram1

    outA1 = maybe (ram0 `Seq.index` addrA_) (const prevA) wroteA

    outB1 = maybe (ram1 `Seq.index` addrB_) (const prevB) wroteB

    outA2 = if enA_ && isRight (isX addrA_) then outA1 else prevA
    outB2 = if enB_ && isRight (isX addrB_) then outB1 else prevB

    (as2,bs2) = go ram2 as1 bs1 outA2 outB2


topEntity
  :: "clk" ::: Clock System
  -> "en"  ::: Signal System Bool
  -> "weA" ::: Signal System Bool
  -> "addrA" ::: Signal System (Index 1024)
  -> "dataA" ::: Signal System (Unsigned 72)
  -> "weB" ::: Signal System Bool
  -> "addrB" ::: Signal System (Index 1024)
  -> "dataB" ::: Signal System (Unsigned 72)
  -> "out" ::: (Signal System (Unsigned 72), Signal System (Unsigned 72))
topEntity = ultraRam# "ultra"

{-# NOINLINE topEntity #-}
makeTopEntity 'topEntity

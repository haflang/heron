{-# LANGUAGE AllowAmbiguousTypes #-}

-- TODO Parameterise this by ram primitive. Although it currently synthesises to
-- distributed ram, we want to ensure that is always the case!

{-| A parallel stack structure with access to \( 2^p \) contiguous elements
  simultaneously. Based on the octostack implementation from
  [Reduceron](https://www.cs.york.ac.uk/fp/reduceron/).
-}
module Heron.Core.ParStack
  ( -- *  Generation
    newCachedParStack
    -- * ParStack Types
  , Offset
  , PSAddr
  , PSIn
  , PSOut(..)
  , ParStack
    -- * Helpers
  , top
  ) where

import Clash.Prelude
import Data.Maybe (isJust, fromJust)

import Heron.Core.Types
import Heron.Xilinx.DistributedRam

-- | The shifting offset for our \( 2^p \) banks of sub-memories
type Offset p = Signed (1 + p)

-- | The virtual address, as used by the caller
type PSAddr d = Index d

-- | The address within each sub-memory
type RamAddr p d = Unsigned (CLog 2 d - p)

-- | ParStack input
type PSIn a p
  = Maybe (Offset p
          ,Vec (2^p) (Maybe a))
  -- ^ A push offset (implicitly controlling how many elements are popped) and a
  -- `Vec` of new elements to push.

-- | ParStack output
data PSOut a p d
  = PSOut
  { _size  :: PSAddr d -- ^ Current stack size
  , _size' :: PSAddr d -- ^ New stack size after pending operation
  , _tops  :: Vec (2^p) (Maybe a) -- ^ Top \( 2^p \) stack elements
  } deriving (Show, Generic, NFDataX, ShowX)

-- | Return just the top atom from the stack
top :: PSOut a p d -> a
top = fromJust . head . (++ singleton Nothing) . _tops

instance SizedRead (PSOut a p d) where
  type SizedAddr (PSOut a p d) = PSAddr d
  type SizedData (PSOut a p d) = Vec (2^p) (Maybe a)
  size (PSOut sz _ _) = sz
  read (PSOut _ _ x) = x

-- | A parallel stack contains elements of type `a`,
--   with simultaneous access to the top `p` elements,
--   and a full depth of `d` elements.
type ParStack dom a p d = Signal dom (PSIn a p) -> Signal dom (PSOut a p d)

-- | Parallel stack /without/ extra caching of top \( 2^p \) entries
newParStack
  :: forall p d a dom .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , HiddenClockResetEnable dom
     , 1 <= d
     , 1 <= d `Div` 2^p
     , 1 <= 2^p
     , p <= CLog 2 d )
  => ParStack dom a p d
newParStack ins = PSOut <$> fmap bitCoerce sp <*> fmap bitCoerce sp' <*> bundle rOuts
  where
    -- Resolve input fields
    pushes    = unbundle (maybe (repeat Nothing) snd <$> ins)
    offset    = maybe 0 fst  <$> ins
    offsetExt = resize <$> offset :: Signal dom (Signed (1+CLog 2 d))

    -- Stack size tracking
    sp  = delay (0 :: Unsigned (CLog 2 d)) sp'
    sp' = wrapAddr @d $
          (bitCoerce . resize) <$> (offsetExt + (bitCoerce . resize <$> sp))

    -- Rotation required for top block (inspect LSBs)
    rotVal = delay 0 rotVal'
    rotVal' = resize <$> sp' :: Signal dom (Unsigned p)

    -- Generate RAM inputs
    addrs = unbundle $ delay (iterateI (\x->x-1) (-1))
                             (bundle addrs')
    addrs' = map (\a -> wrapAddr @d $ offsetExt + a) addrs
    ramAddrs = map (fmap (msbs . bitCoerce . resize)) addrs'

    msbs :: Unsigned (CLog 2 d) -> RamAddr p d
    msbs x = resize $ x `shiftR` snatToNum (SNat :: SNat p)

    wElems = reverse .
             unbundle $
             liftA2 rotateLeft
                    (bundle pushes)
                    rotVal'

    wCtrls = zipWith (liftA2 (\a e -> ((a,) . Just) <$> e))
                     ramAddrs
                     wElems

    -- Gather RAM outputs
    rElems = zipWith (readNew (blockRam1 ClearOnReset
                                         (SNat :: SNat (d `Div` (2^p)))
                                         Nothing))
                     ramAddrs
                     wCtrls
    rOuts = unbundle $
            liftA2 rotateRight
                   (bundle $ reverse rElems)
                   rotVal

-- | Parallel stack /with/ extra caching of top \( 2^p \) entries
newCachedParStack
  :: forall p d a dom .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , HiddenClockResetEnable dom
     , 1 <= d
     , 1 <= d `Div` 2^p
     , 1 <= 2^p
     , p <= CLog 2 d )
  => ParStack dom a p d
newCachedParStack ins = PSOut <$> sp <*> sp' <*> bundle cache
  where
    -- Resolve input fields
    pushes   = unbundle (maybe (repeat Nothing) snd <$> ins)
    offset   = maybe 0 fst <$> ins
    offsetExt = resize <$> offset :: Signal dom (Signed (1+CLog 2 d))

    -- Inst BRAM parallel stack
    ramPushMask = map (\i -> i .<. offsetExt .&&. offset .>. 0)
                      (iterateI (+1) 0)
    ramPushes = zipWith (\m e -> mux m e (pure Nothing)) ramPushMask rotated
    ramStack = newParStack @p @d (Just <$> bundle (offset, bundle ramPushes))
    sp      = (\(PSOut x _ _) -> x) <$> ramStack
    sp'     = (\(PSOut _ x _) -> x) <$> ramStack
    ramOuts = unbundle $
              (\(PSOut _ _ x) -> x) <$> ramStack

    -- Rotated versions versions
    rotated = unbundle $
              liftA2 rotateRight
                     (bundle cache)
                     offset

    ramTops = unbundle $
              liftA2 rotateRight
                     (bundle ramOuts)
                     offset

    -- Select source for each cache register
    popMask  = reverse $ map (\i -> negate i .>. offsetExt)
                             (iterateI (+1) 0)
    pushMask = map (fmap isJust) pushes

    cache  = map (delay Nothing) cache'
    cache' = zipWith5 (liftA5 choose) pushMask popMask rotated ramTops pushes
    -- TODO What about Octostack's `f` parameter?

choose :: Bool -> Bool -> p -> p -> p -> p
--     psh? pop? rot  ram  push
choose _    True _    vRam _     = vRam
choose True _    _    _    vPush = vPush
choose _    _    vRot _    _     = vRot

liftA5 :: Applicative f
       => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
       -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f b
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

wrapAt :: ( HiddenClockResetEnable dom
          , Num a
          , Ord a                      )
       => Signal dom a -> Signal dom a -> Signal dom a
wrapAt limit x = mux (x .>. limit) (x-limit) x

wrapAddr :: forall d a dom
          . ( HiddenClockResetEnable dom
            , KnownNat d
            , 1 <= d
            , Num a
            , Ord a                      )
         => Signal dom a -> Signal dom a
wrapAddr addr = wrapAt (snatToNum $ SNat @(d-1)) addr

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

module Heron.Schedule.Put where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Heron.Core.Heap      ()
import           Heron.Schedule.Types
import           Heron.Template
import           RetroClash.Barbies

data PutCmd = PutCmd
  { withIndirection :: Bool -- | The sender holds a reference to this, increment RC
  , srcAddr         :: GlobalAddr   -- | Source address
  , dstAddr         :: HeapAddr -- | Destination local address
  , node            :: HeapNode -- | New heap node
  } deriving (Show,ShowX,Generic,NFDataX, BitPack)

declareBareB [d|
  data PutIn = PutIn
    { cmd :: Maybe PutCmd
    , localWriteReady :: Bool
    , coreId :: CoreId
    } |]
deriving instance Generic (Pure PutIn)
deriving instance NFDataX (Pure PutIn)
deriving instance Show    (Pure PutIn)
deriving instance ShowX   (Pure PutIn)

declareBareB [d|
  data PutOut = PutOut
    { _localWritePush :: Maybe ComMutRequest
    , _cmdPop :: Bool
    } |]
makeLenses ''PutOut
deriving instance Generic (Pure PutOut)
deriving instance NFDataX (Pure PutOut)
deriving instance Show    (Pure PutOut)
deriving instance ShowX   (Pure PutOut)

comPut
  :: HiddenClockResetEnable dom
  => Signals dom PutIn -> Signals dom PutOut
comPut = bunbundle . fmap go . bbundle
  where
    idle = PutOut Nothing False

    resolve c _ i (FetchMe (c',addr))
      | c==c' = CMPut False i c' $ App False 1 $ Just (Ptr PShared addr) :> repeat Nothing
    resolve _ srcCid i n = CMPut True i srcCid n

    go (PutIn {..})
      | localWriteReady = case cmd of
          Nothing -> idle
          Just (PutCmd indr (cid',_) _ node) ->
            let putNode = resolve coreId cid' indr node
            in PutOut (Just putNode) True
      | otherwise = idle

-- TODO We really need all child pointers to be given a GA too... if they are
-- collected before being used, their parent RC will never be decremented. Best
-- place for this is in MUT, so we only traverse each child once. But that needs
-- an extra port for RC memory. We could do it during GC marking? we mark a
-- FetchMe, we set its GA? Nah, because some might be indirections? Is that OK?

-- Might want to move the rc memory access from Put.hs to Core.hs

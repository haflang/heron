module Heron.Error where

import           Clash.Prelude
import           Clash.Prelude.Testbench (assertBitVector)
import           Heron.Template (CoreId)

data Err
  -- * Architecture erros
  = ErrHeapFull
  -- * Mutator errors
  | ErrMutNotRedex
  | ErrMutResumeFormat
  | ErrMutResumeNode
  | ErrMutUnwindTSO
  | ErrMutUnwindNonPtr
  | ErrMutEmptyUStk
  | ErrMutSimpleAltFn
  | ErrMutDoubleUpdate
  -- * Memory collisions
  | ErrMemCollisionWW
  | ErrMemCollisionRW
  | ErrMemCollisionWR
  -- * GC errors
  | ErrGcNoTail
  | ErrGcWorkDuringSweep
  | ErrGcSparkDuringTsoRoots
  | ErrGcTsoDuringSparkSweep
  | ErrGcBadFree
  deriving (Eq,BitPack,Generic,NFDataX,Show,ShowX)

assertError
  :: forall dom a . HiddenClockResetEnable dom
  => Signal dom CoreId -> Signal dom (Maybe Err) -> Signal dom a -> Signal dom a
assertError cid e =
  assertBitVector "Caught Heron runtime error"
    (pack <$> bundle (cid,e))
    (pack <$> (bundle (cid,pure Nothing) :: Signal dom (CoreId, Maybe Err)))

assertError'
  :: HiddenClockResetEnable dom
  => Signal dom CoreId -> Signal dom (Maybe Err) -> Signal dom (Maybe Err)
assertError' cid e = assertError cid e e

decodeErr :: Int -> (CoreId, Err, String)
decodeErr i = (cid,e,msg)
  where
    (cid,e) = unpack (fromIntegral i)
    msg = explainErr e

explainErr :: Err -> String
explainErr e = case e of
  ErrHeapFull ->
    "Heap memory is exhausted with live data"
  ErrMutNotRedex ->
    "Top of mutator stack is not a valid redex"
  ErrMutResumeFormat ->
    "Mutator resume expects one pointer on top of stack (prevents GC progressing past stack root ID phase)"
  ErrMutResumeNode ->
    "Mutator found graph data node when resuming (expects only TSO objects)"
  ErrMutUnwindTSO ->
    "Mutator found TSO object while trying to unwind graph data"
  ErrMutUnwindNonPtr ->
    "Mutator tried to unwind a non-pointer atom"
  ErrMutEmptyUStk ->
    "Mutator tried to update with an empty update stack"
  ErrMutSimpleAltFn ->
    "Mutator tried to instantiate a simple case alternative but found a function"
  ErrMutDoubleUpdate ->
    "Mutator updated an address that was already WHNF. Something likely went wrong in GC."
  ErrMemCollisionWW ->
    "Write-Write address collision in dual-port memory"
  ErrMemCollisionRW ->
    "Read-Write address collision in dual-port memory"
  ErrMemCollisionWR ->
    "Write-Read address collision in dual-port memory"
  ErrGcNoTail ->
    "GC tried to follow a tail pointer from an (un)marked node"
  ErrGcWorkDuringSweep ->
    "GC found a worklist element during sweeping"
  ErrGcSparkDuringTsoRoots ->
    "GC read a spark address during TSO root ID"
  ErrGcTsoDuringSparkSweep ->
    "GC read a TSO address during spark sweeping"
  ErrGcBadFree ->
    "GC found a non-free element on the freelist"

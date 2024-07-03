{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-| The Heron Core's control logic. This captures the core's combinatorial logic
  for performing reductions and controlling execution phases (reduction, GC,
  stalls, etc.).

  The style is heavily inspired by [Gergő Érdi's Retrocomputing with Clash
  book](https://erdi.dev/retroclash/). Use of @barbies@ is to let us use just
  /one/ record type for both combinatorial logic and collections of synchronous
  signals.
-}
module Heron.Core.Core
  ( CPUIn(..)
  , CPUOut(..)
  , CPUStats(..)
  , Phase(..)
  , cpu
  ) where

import           Barbies.TH
import           Clash.Annotations.BitRepresentation.Deriving
import           Clash.Prelude                                hiding (fail,
                                                               read)
import           Control.Lens                                 hiding (Index,
                                                               assign, at, imap,
                                                               op, (:>))
import           Control.Monad.State                          hiding (fail)
import           Data.Maybe                                   (fromMaybe)
import qualified Prelude                                      as P
import           RetroClash.Barbies
import           RetroClash.CPU                               hiding (update)

import           Heron.Core.Alu
import           Heron.Core.Collector                         (GCCmd (..),
                                                               GCRequest (..))
import           Heron.Core.Heap
import           Heron.Core.ParStack
import           Heron.Core.Rom
import           Heron.Core.Stack
import           Heron.Core.Types
import           Heron.Parameters
import           Heron.Template

-- CPU data types

-- | CPU input record. Includes outputs from all memory components, and a
-- `begin` trigger.
declareBareB [d|
  data CPUIn = CPUIn
    { uStkIn :: SOut    (Index VStkSize, HeapAddr) UStkSize
    -- ^ Size and head of update stack
    , aStkIn :: SOut    (CaseTable UnpackedAlt   ) AStkSize
    -- ^ Size and head of case alternative stack
    , pStkIn :: SOut    PInt                       PStkSize
    -- ^ Size and head of primitive alternative stack
    , vStkIn :: PSOut   Atom      Log2MaxPush      VStkSize
    -- ^ Size and head of parallel value stack
    , heapIn :: HeapOut HeapNode  MaxAps           HeapSize
    -- ^ Applications read from heap ports (may be `undefined` after no-op)
    , tmplIn :: RomOut  Template
    -- ^ Template read from program memory
    , nextAddrs :: Vec MaxAps HeapAddr
    -- ^ Next free addresses for allocation
    , gcCmd     :: GCCmd
    -- ^ Command requested from GC core. Required to stall mutator when heap is
    -- full or ask the mutator to dump the graph roots when starting a GC pass.
    -- ^ Size and head of parallel value stack
    , begin     :: Maybe   TemplAddr
    -- ^ Start reduction of main function
    } |]
deriving instance Generic (Pure CPUIn)
deriving instance NFDataX (Pure CPUIn)
instance Show (Pure CPUIn) where
  show CPUIn{..} = unlines
    [ ""
    , "  uStk = " P.++ show uStkIn
    , "  aStk = " P.++ show aStkIn
    , "  pStk = " P.++ show pStkIn
    , "  vStk = " P.++ show vStkIn
    , "  heap = " P.++ show heapIn
    , "  tmpl = " P.++ show tmplIn
    , "  fls  = " P.++ show nextAddrs
    , "  go   = " P.++ show begin
    ]
instance ShowX (Pure CPUIn) where
  showX CPUIn{..} = unlines
    [ ""
    , "  uStk = " P.++ showX uStkIn
    , "  aStk = " P.++ showX aStkIn
    , "  pStk = " P.++ showX pStkIn
    , "  vStk = " P.++ showX vStkIn
    , "  heap = " P.++ showX heapIn
    , "  tmpl = " P.++ showX tmplIn
    , "  fls  = " P.++ showX nextAddrs
    , "  go   = " P.++ showX begin
    ]

-- | Statistics collected from mutator runtime. These allow us to distinguish
-- mutator cycles from GC overhead.
data CPUStats = CPUStats
  { _mutCycles    :: Unsigned 32
  -- ^ Cycles spent doing useful mutation
  , _gcRootCycles :: Unsigned 32
  -- ^ Paused cycles spent dumping graph roots
  , _gcWaitCycles :: Unsigned 32
  -- ^ Paused cycles spent waiting for more free addresses
  , _maxStall     :: Unsigned 32
  -- ^ Worst-case pause time
  } deriving (Show, ShowX, Generic, NFDataX, BitPack)
makeLenses ''CPUStats

initCPUStats :: CPUStats
initCPUStats = CPUStats 0 0 0 0

-- | CPU output record. Includes inputs for all memory components, and the
-- `_result` node.
declareBareB [d|
  data CPUOut = CPUOut
    { _uStkPush  :: Maybe (Index VStkSize, HeapAddr)
    -- ^ Push element to update stack
    , _uStkPop   :: Bool
    -- ^ Pop element from update stack
    , _aStkPush  :: Maybe (CaseTable UnpackedAlt)
    -- ^ Push element to case alternative stack
    , _aStkPop   :: Bool
    -- ^ Pop element from case alternative stack
    , _pStkPush  :: Maybe PInt
    -- ^ Push element to primitive stack
    , _pStkPop   :: Bool
    -- ^ Pop element from primitive stack
    , _vStkOut   :: PSIn   Atom      Log2MaxPush
    -- ^ Pop _and_ push to parallel value stack
    , _heapOut   :: HeapIn HeapNode  MaxAps         HeapSize
    -- ^ Heap operations
    , _tmplOut   :: RomIn  RomSize
    -- ^ Program memory operation
    , _updateAddr :: Maybe HeapAddr
    -- ^ The address of a shared application that has been mutated
    , _gcRequest     :: GCRequest
    -- ^ Graph roots for the GC snapshot (any heap pointer on the main value stack)
    , _allocBubble :: Bool
    -- ^ A flag raised when we are certain no allocations will happen on the next cycle
    , _result    :: (CPUStats, Maybe Atom)
    -- ^ The final result. Always a single primitive integer on success.
    } |]
makeLenses ''CPUOut
deriving instance Generic (Pure CPUOut)
deriving instance NFDataX (Pure CPUOut)

instance Show (Pure CPUOut) where
  show CPUOut{..} = unlines
    [ ""
    , "  uStk  = " P.++ show _uStkPush P.++ " " P.++ show _uStkPop
    , "  aStk  = " P.++ show _aStkPush P.++ " " P.++ show _aStkPop
    , "  pStk  = " P.++ show _pStkPush P.++ " " P.++ show _pStkPop
    , "  vStk  = " P.++ show _vStkOut
    , "  heap  = " P.++ show _heapOut
    , "  tmpl  = " P.++ show _tmplOut
    , "  gcReq = " P.++ show _gcRequest
    , "  res   = " P.++ show _result
    ]
instance ShowX (Pure CPUOut) where
  showX CPUOut{..} = unlines
    [ ""
    , "  uStk  = " P.++ showX _uStkPush P.++ " " P.++ showX _uStkPop
    , "  aStk  = " P.++ showX _aStkPush P.++ " " P.++ showX _aStkPop
    , "  pStk  = " P.++ showX _pStkPush P.++ " " P.++ showX _pStkPop
    , "  vStk  = " P.++ showX _vStkOut
    , "  heap  = " P.++ showX _heapOut
    , "  tmpl  = " P.++ showX _tmplOut
    , "  gcReq = " P.++ showX _gcRequest
    , "  res   = " P.++ showX _result
    ]

-- | Current CPU execution phase
data Phase
  = Init      -- ^ Initialising
  | Starting  -- ^ Waiting for initialisation
  | Reduce    -- ^ Performing reductions
  | Stall     -- ^ Stalling until heap port becomes available
  | GCStall     -- ^ Stalling until heap space is freed
  | Halt      -- ^ Finished reducing
  | DumpRoots -- ^ Dumping GC roots from primary stack
  | ContinueUpdate -- ^ Extra cycle needed for wide updates
  deriving (Show, Generic, NFDataX, Enum, Bounded, Eq)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| Phase |]
deriveBitPack [t| Phase |]

data CPUState = CPUState
  { _phase         :: Phase
  , _top'          :: Atom
  , _alt'          :: Maybe (CaseTable UnpackedAlt)
  , _regs          :: Vec MaxRegs Atom
  , _frozenArgs    :: Vec CMaxPush Atom
  , _forwardedNode :: Maybe HeapNode
  , _allocs        :: Vec MaxApSpan HeapAddr
  , _stalls        :: Unsigned 2
  , _wideUpd       :: (HeapAddr, HeapNode)
  , _stats         :: CPUStats
  , _curStall      :: Unsigned 32
  }
  deriving (Generic, NFDataX)
makeLenses ''CPUState

initCPUState :: CPUState
initCPUState = CPUState
  { _phase = Init
  , _top'  = PrimInt 0
  , _alt'  = Nothing
  , _regs  = repeat (PrimInt 0)
  , _frozenArgs    = repeat (unpack 0)
  , _forwardedNode = Nothing
  , _allocs = repeat undefined
  , _wideUpd = (0, unpack 0)
  , _stalls = 0
  , _stats = initCPUStats
  , _curStall = 0
  }

defaultOutput :: CPUState -> Pure CPUOut
defaultOutput CPUState{..} = CPUOut
  { _uStkPush = Nothing
  , _uStkPop  = False
  , _aStkPush = Nothing
  , _aStkPop  = False
  , _pStkPush = Nothing
  , _pStkPop  = False
  , _vStkOut = Nothing
  , _heapOut = heapOp _top'      -- Fetch when top is pointer (INV2)
  , _tmplOut = romOp _top' _alt' -- Fetch when top is FUN/CON (INV1)
  , _updateAddr = Nothing
  , _gcRequest = RNothing
  , _allocBubble = hasBubble _top'
  , _result  = (_stats, Nothing)
  }
  where
    heapOp = maybe (repeat RamNoOp) (\a -> repeat RamNoOp ++ singleton (RamRead a)) . heapAddr
    romOp  (Fun _ a _) _                              = a
    romOp  (Con _ tag) (Just (CTOffset alt         )) = alt + bitCoerce (resize tag)
    romOp  (Con _ tag) (Just (CTInline (UAFun alt) _))
      | lsb tag == low  = alt
    romOp  (Con _ tag) (Just (CTInline _ (UAFun alt)))
      | lsb tag == high = alt
    romOp  _             _                            = 0
    hasBubble _
      | _phase == Stall     = True
      | _phase == GCStall   = True
      | _phase == DumpRoots = True
      | _phase == ContinueUpdate = False
    hasBubble (Fun {}) = False
    hasBubble (Con {})   = False
    hasBubble _           = True

-- | Synchronous control logic, packaged as a Mealy machine
cpu :: (HiddenClockResetEnable dom)
    => Signals dom CPUIn -> Signals dom CPUOut
cpu = mealyCPU initCPUState defaultOutput step

type CPU = CPUM CPUState CPUOut

-- Dispatch for CPU phases

step :: Pure CPUIn -> CPU ()
step ins@CPUIn{..} =
  use phase >>= \case
    Halt   -> do
      s <- use stats
      result .:= (s, Just . head $ read vStkIn)
      gcRequest .:= RFinished
      phase   .= Init
    Init   ->
      case begin of
        Nothing       -> pure ()
        Just initAddr ->
          phase .= Starting >>
          stats .= initCPUStats >>
          updateV 0 (Just (Fun 0 initAddr True) :> repeat Nothing)
    Starting -> do
      unless pause (phase .= Reduce)
    Stall ->
      stats . mutCycles %= (+1) >>
      stall
    GCStall ->
      stats . gcWaitCycles %= (+1) >>
      curStall %= (+1) >>
      unless pause (phase .= Reduce)
    Reduce ->
      latchMaxStall >>
      curStall %= (+1) >>
      if fail  then updateV 1 (Just gcErrCode :> repeat Nothing) >>
                    phase .= Halt else
      if halt  then phase .= Halt else
      if pause then phase .= GCStall >>
                    stats . gcWaitCycles %= (+1) else
      if gc    then phase .= DumpRoots >>
                    stats . gcRootCycles %= (+1) else
      stats . mutCycles %= (+1) >>
      top' .= top vStkIn >>
      alt' .= _newTop aStkIn >>
      curStall .= 0 >>
      reduce (top vStkIn) ins
    DumpRoots ->
      stats . gcRootCycles %= (+1) >>
      curStall %= (+1) >>
      dumpRoots ins
    ContinueUpdate ->
      stats . gcWaitCycles %= (+1) >>
      continueUpdate
  where
    fail     = gcCmd == FailCmd
    pause    = gcCmd == WaitCmd
    gc       = gcCmd == RootsCmd && safeInterrupt
    halt     = size vStkIn < 1 && isInt (top vStkIn)
    safeInterrupt = canGC $ top vStkIn
    gcErrCode = Con maxBound maxBound
    latchMaxStall = do n <- use curStall
                       stats . maxStall %= max n

-- Dispatch for reduction rules

reduce :: Atom -> Pure CPUIn -> CPU ()
reduce t ins
  | needsUnwind t      = unwind ins
  | needsUpdate t  ins = update ins
  | needsUnfold t  ins = unfold ins
reduce (Con   _ _) ins = caseSelect ins
reduce (PrimInt _) ins = prim ins
reduce t _   =
  error $ "Sim.Core.reduce: Unexpected top atom." P.++ show t

-- Reduction rules

-- Unwind a heap application onto the stack
unwind :: Pure CPUIn -> CPU ()
unwind CPUIn{..} =
  do t <- use top'

     let haddr = case heapAddr t of
                   Nothing -> errorX "Heron.Core.Core.unwind: Found empty heap address"
                   Just x  -> x
     let shared = isShared t

     -- Node to unwind is either forwarded via CPUState or prefetched from heap.
     fwd <- use forwardedNode
     forwardedNode .= Nothing
     let node = fromMaybe (last $ read heapIn) fwd
     let unode = unpackNode shared node

     -- Possibly register update address
     when (nUpdatable unode)
          (pushU (size vStkIn, haddr))

     -- Possibly register case table
     forM_ (nCaseTable unode) pushA

     -- Free any non-shared unwound apps
     unless shared
          (gcRequest .:= RDealloc haddr)

     -- Push atoms onto stack
     let offset = (unpack . resize . pack $ nArity unode) - 1
     updateV offset
             (map (fmap (dashIf shared)) (nAtoms unode))

-- Perform primitive operations
prim :: Pure CPUIn -> CPU ()
prim CPUIn{..} =
  case primOpPat (map Just (read vStkIn)) of
    -- Args already evaled; perform op
    BothInt x (_, swp, op) y ->
      updateV (-2) (Just (doOp op swp x y) :> repeat Nothing)

    -- Both args now evaluated but first is on prim stack; perform op
    SndInt y (_, swp, op) ->
      case _top pStkIn of
        Nothing -> errorX "Heron.Core.Core.prim: Expected to find element on primitive stack"
        Just x  -> popP >>
                   updateV (-1) (Just (doOp op swp x y) :> repeat Nothing)

    -- Second arg unevaluated; push first onto prim stack
    FstInt x y ->
      pushP x >>
      updateV (-1) (Just y :> repeat Nothing)

    -- Special case for SEQ
    Seq x f ->
      updateV (-1) (Just f :> Just (PrimInt x) :> repeat Nothing)

    -- Unexpected prim op pattern
    NotPrim -> error $ "Core.Core.prim: Malformed args on value stack: " <> show (read vStkIn)
  where
    doOp op swp a b = alu $ AluIn op swp a b

-- Update a heap node with its normal form
update :: Pure CPUIn -> CPU ()
update CPUIn{..} = case read uStkIn of
  Nothing ->
    error "Core.Core.update: Read from empty update stack"
  Just (_,uAddr) -> do

    --Dash the NF application on the stack, marking it as possibly shared
    let stkAs   = read vStkIn
    let nfArity = atomArity (head stkAs)
    let stkAs'  = imap (\i a -> if i < resize nfArity
                                  then Just (dash a)
                                  else Nothing) stkAs
    updateV 0 stkAs'
    popU

    -- Write the normal form to heap
    if nfArity <= nodeLen

      -- Small normal form: Fits in one heap node
      then
        let n = App True (resize nfArity) (takeI stkAs') in
        if gcCmd == UpdateBarrierCmd
          then
            specialiseHeap heapConfig
              -- UltraRAM
              (heapOut .:= RamRead uAddr :> RamWrite uAddr n :> repeat RamNoOp)
              -- BlockRAM
              (heapOut .:= RamWrite uAddr n                  :> repeat RamNoOp) >>
            updateAddr .:= Just uAddr
          else
            heapOut .:= RamNoOp       :> RamWrite uAddr n :> repeat RamNoOp

      -- Large normal form: Split into two heap nodes
      -- e.g. NF [a,b,c,d,e,f] -> x |-> [a,b,c,d]; y |-> [@x, e, f]
      -- We don't need to worry about the bubble signal here. Normal forms
      -- are either function apps, constructor apps, prim op apps, or integers.
      -- Function and constructor cases are already handled.
      -- Prim ops and ints are never wide enough to merit an allocation during update.
      else
        let n1 = App True (resize nodeLen) (takeI stkAs')
            n1Addr = head nextAddrs
            n2Args = takeI $ dropI @NodeLen stkAs' ++
                             repeat @NodeLen Nothing
            n2 = App True (resize $ nfArity + 1 - nodeLen)
                     (Just (Ptr True n1Addr) :> n2Args)
        in gcRequest .:= RAlloc (True :> repeat False) >>
           if gcCmd == UpdateBarrierCmd
             then
               specialiseHeap heapConfig
                 -- UltraRAM
                 (wideUpd .= (n1Addr, n1)   >>
                  phase   .= ContinueUpdate >>
                  heapOut .:= RamRead uAddr :> RamWrite uAddr n2 :> repeat RamNoOp)
                 -- BlockRAM
                 (heapOut .:= RamWrite uAddr n2 :> RamWrite n1Addr n1 :> repeat RamNoOp) >>
               updateAddr .:= Just uAddr
             else
               heapOut .:= RamWrite uAddr n2 :> RamWrite n1Addr n1 :> repeat RamNoOp
  where
    nodeLen = snatToNum (SNat @NodeLen)

-- Instantiate a template
unfold :: Pure CPUIn -> CPU ()
unfold CPUIn{..} =
  do -- If we came from a case expression, pop from alternative stack
     let t = head $ read vStkIn
     when (isCon t)
          popA

     -- Freeze args when starting new chain of split templates
     when (newTmplChain t)
          (frozenArgs .= takeI (read vStkIn))
     args <- use frozenArgs

     -- Instantiate all template atoms
     -- Resolves ARGs, REGs, and PTRs
     primRegs <- use regs
     prevAddrs <- use allocs
     let curAddrs = nextAddrs
     let Template pushOffset spineAp heapAps =
           mapTemplate (inst prevAddrs curAddrs args primRegs)
                       tmplIn

     -- Push spine ap to stacks
     let uSpineAp = unpackNode False spineAp
     updateV pushOffset (nAtoms uSpineAp)
     when (nAtoms uSpineAp == repeat Nothing)
          (top' .= at d1 (read vStkIn))

     maybe (return ()) pushA (nCaseTable uSpineAp)

     -- Heap prefetching logic, using forwarding for references to newly
     -- allocated nodes
     let headPtr =
           head (nAtoms . unpackNode @MaxPush False $ tSpine tmplIn) >>= heapAddr
     let defReadCtrl = maybe RamNoOp RamRead $
                       head (nAtoms uSpineAp) <|> Just (at d1 (read vStkIn)) >>= heapAddr
     let (readCtrl, fwdNode)
           = case headPtr of
               Just addr -> (if addr < fromSNat (lengthS heapAps) then (RamNoOp, heapAps !! addr) else (defReadCtrl, Nothing))
               Nothing   -> (defReadCtrl, Nothing)
     forwardedNode .= fwdNode

     -- Allocate heap aps
     let (regWrs, hWrs) = unzip $ zipWith allocAp curAddrs heapAps
     -- TODO could model regs as a tiny TDP LUTRAM with async read?
     regs .= foldr updReg primRegs regWrs
     -- let heapWrs = foldl shiftHeapOps (repeat Nothing) (reverse hWrs)
     prevAllocs <- use allocs
     allocs .= foldl shiftAllocBuf prevAllocs hWrs
     arbitrateHeap readCtrl hWrs
     gcRequest .:= RAlloc (map wr hWrs)
  where
    newTmplChain (Fun _ _ False) = False
    newTmplChain _               = True
    isReducible (BothInt x (_, _, op) y) = Just (x,op,y)
    isReducible _                        = Nothing

    allocAp addr (Just (Prim reg _ as)) =
      case isReducible (primOpPat as) of
        -- If arguments are ints, reduce op now; no heap allocation
        Just (x, op, y) -> (Just (reg, alu $ AluIn op False x y), RamNoOp)
        -- Otherwise, dump onto heap
        Nothing -> (Just (reg, Ptr False addr), RamWrite addr (App False 3 (as ++ repeat Nothing)))
    allocAp addr (Just app) = (Nothing, RamWrite addr app)
    allocAp _ Nothing = (Nothing, RamNoOp)

    updReg (Just (i, n)) = replace i n
    updReg Nothing       = id

    shiftAllocBuf ops (RamWrite i _) = i +>> ops
    shiftAllocBuf ops _              = ops

    isNoOp RamNoOp = True
    isNoOp _       = False

    -- If we can't prefetch from the heap, we might need to stall
    arbitrateHeap rd wrs = do
      let (opDo, opSkip) = orderRamOp (last wrs) rd
      heapOut .:= (init wrs ++ singleton opDo)
      phase .= if isNoOp opSkip then Reduce else Stall

    orderRamOp RamNoOp  b = (b, RamNoOp)
    orderRamOp a        b = (a, b)

    wr (RamWrite _ _) = True
    wr _              = False

-- Instantiate simple, non-function valued inline case alternatives
caseSelect :: Pure CPUIn -> CPU ()
caseSelect CPUIn{..} =
  do let args = takeI @CMaxPush (read vStkIn)
     let ct   = _top aStkIn
     t <- use top'
     popA

     -- Select correct alternative
     case getAlt (t, ct) of
       Just (n, x, y) ->
         do let alt = if bitToBool (lsb n) then y else x

            -- Instantiate alt on vstack
            let (offset, atom) = instAlt args alt
            updateV offset (Just atom :> repeat Nothing)
       Nothing -> error "Core.Core.caseSelect: malformed scrutinee"
  where
    getAlt (Con _ n, Just (CTInline x y)) = Just (n, x, y)
    getAlt _                              = Nothing

dumpRoots :: Pure CPUIn -> CPU ()
dumpRoots CPUIn{..} =
  do gcRequest .:= RRoot (read vStkIn)
     when (gcCmd /= RootsCmd)
          (phase .= Reduce)

continueUpdate :: CPU ()
continueUpdate = do
  (addr, node) <- use wideUpd
  heapOut .:= RamWrite addr node :> repeat RamNoOp
  phase .= Reduce

stall :: CPU ()
stall = use stalls >>= \case
  0 -> phase  .= Reduce
  n -> stalls .= n-1

-- Helper functions

needsUnwind :: Atom -> Bool
needsUnwind (Ptr _ _) = True
needsUnwind _         = False

needsUpdate :: Atom -> Pure CPUIn -> Bool
needsUpdate t CPUIn{..}
  = maybe False go (read uStkIn)
  where
    go (uLen, _)
      = resize (atomArity t) > satSub SatBound (size vStkIn) uLen

needsUnfold :: Atom -> Pure CPUIn -> Bool
needsUnfold t CPUIn{..} = go t (_top aStkIn)
  where
    go (Fun {}) _ = True
    go (Con _ _) (Just (CTOffset _)) = True
    go (Con _ tag) (Just (CTInline (UAFun _) _))
      | lsb tag == low  = True
    go (Con _ tag) (Just (CTInline _ (UAFun _)))
      | lsb tag == high = True
    go _ _ = False

-- Instantiate an atom relative to current heap pointer, top spine values, and
-- register contents
inst :: Vec MaxApSpan HeapAddr -> Vec MaxAps HeapAddr -> Vec CMaxPush Atom -> Vec MaxRegs Atom ->
        Atom -> Atom
inst _ _ spine _ (Arg shared indx)
  = let arg = spine !! (1 + resize indx :: Index CMaxPush)
    in dashIf shared arg
inst prevAddrs curAddrs _ _ (Ptr shared addr)
  | addr < snatToNum (SNat @MaxAps) = Ptr shared $ curAddrs !! addr
  | otherwise -- We need to look back at recent allocations
  = Ptr shared $ prevAddrs !! (negate addr - 1)
inst _ _ _ regsv (Reg shared indx)
  = dashIf shared $ regsv !! indx
inst _ _ _ _ a = a

-- Instantiate an atom relative to current heap pointer, top spine values, and
-- register contents
instAlt :: Vec CMaxPush Atom -> UnpackedAlt -> (Offset Log2MaxPush, Atom)
instAlt _    (UAFun _) = error "Core.Core.instAlt: Unexpected function alt in caseSelect"
instAlt _    (UAInt pops val      ) = (altPushOffset pops, PrimInt   (resize val))
instAlt _    (UACon pops arity tag) = (altPushOffset pops, Con arity (resize tag))
instAlt args (UAArg pops idx      ) = (altPushOffset pops, args !! i)
  where i = 1 + resize idx :: Index CMaxPush

pushA :: CaseTable UnpackedAlt -> CPU ()
pushA tab = aStkPush .:= Just tab

popA :: CPU ()
popA = aStkPop .:= True

pushU :: (Index VStkSize, HeapAddr) -> CPU ()
pushU u = uStkPush .:= Just u

popU :: CPU ()
popU = uStkPop .:= True

pushP :: PInt -> CPU ()
pushP p = pStkPush .:= Just p

popP :: CPU ()
popP = pStkPop .:= True

updateV :: Offset Log2MaxPush -> Vec CMaxPush (Maybe Atom) -> CPU ()
updateV off as =
  vStkOut .:= Just (off, as ++ repeat Nothing) >>
  forM_ (head as) (top' .=)
  -- Update top' state to inform memory prefetching in `defaultOutput`

{-# LANGUAGE FlexibleInstances #-}

{-| A Clash-friendly, synthesisable representation of Heron Templates.

    At the hardware level, we need fixed bounds on these structures. As it
    stands, we define these bounds via CPP flags. It avoids us needing to
    explicitly pass about a configuration argument throughout most of our
    codebase, but does mean that the parameters are fixed during compile-time
    and will appear as constants in the documentation. ¯\\_(ツ)_/¯
-}
module Heron.Template
  (Len

  -- ** Field Definitions
  , OpCode(..)
  , NodeArity
  , FnArity
  , TemplAddr
  , HeapAddr
  , TSOAddr
  , CoreId
  , GlobalAddr
  , PInt
  , RefCount
  , ShortInt
  , Tag
  , ShortTag
  , ArgIndex
  , RegIndex
  , PushOffset
  , IsSwapped
  , IsShared
  , IsNF
  , IsFirst

  -- * Template Representation
  , Template(..)
  -- ** Applications
  , Node(..)
  , SpineNode
  , HeapNode
  -- ** Atoms
  , Atom(..)
  , PtrTag(..)
  -- ** Case handling
  , Alt(..)
  , CaseTable(..)
  , Update (..)

  -- * Unpacked Representations
  , UnpackedNode(..)
  , unpackNode
  , UnpackedAlt(..)
  , unpackAlt
  , unpackCaseTable

  , Markable
  , nodeChildren

  -- * Helpers
  -- ** Booleans
  , falseAtom
  , trueAtom
  , fromBool

  -- ** Inspections
  , isInt
  , isCon
  , isShared
  , isTagShared
  , isWHNF
  , heapAddr
  , setHeapAddr
  , atomArity
  , appLen
  , rawAdd
  , canGC
  , altPushOffset
  , getAtom
  , blockee
  , isLocked

  -- ** Mutations
  , mapNode
  , setAtoms
  , replaceAtom
  , mapTemplate
  , dash
  , dashIf
  , forcePtrTag

  -- * Pretty printing
  , ppNode
  , ppNodeM
  , ppTemplate

  ) where

import           Clash.Annotations.BitRepresentation.Deriving
import           Clash.Prelude                                hiding (msb)
import           Data.Maybe                                   (isJust)
import           Heron.Parameters

import qualified Prelude                                      as P

-- Compile-time `Natural` parameters
---- Main parameters


--------------------------------------------------------------------------------
-- Template Data

-- Field type definitions
-- | Arity for heap node applications
type NodeArity  = Len      NodeLen
-- | Arity for templates (supercombinators)
type FnArity    = Len      MaxArgs
-- | Template ROM pointer
type TemplAddr  = Index    RomSize
-- | Heap RAM pointer
type HeapAddr   = Index    HeapSize
type TSOAddr    = HeapAddr
-- | Primitive integers
type PInt       = Signed   IntW
-- | Short primitive integers (for inline case tables)
type ShortInt   = Signed   ShortIntW
-- | Reference Counts
type RefCount   = Unsigned RefCountW
-- | Constructor tags
type Tag        = Unsigned TagW
-- | Short constructor tags (for inline case tables)
type ShortTag   = Unsigned ShortTagW
-- | Argument pointer
type ArgIndex   = Index    MaxArgs
-- | Register pointer
type RegIndex   = Index    MaxRegs
-- | Primary stack push offset (net effect on size after pops and pushes)
type PushOffset = Signed   (1 + Log2MaxPush)
-- | Are arguments to primitive application swapped?
type IsSwapped  = Bool
-- | Is this node possibly shared (and will need updating with its normal
-- form)?
type IsShared   = Bool
-- | Is this node already in its normal form? TODO Is storing this any better than
type IsNF       = Bool
-- | Is this template the first in a series of split templates?
type IsFirst    = Bool

-- | Encodes natural numbers from 0 to n, inclusive.
type Len n = Index (1+n)

type CoreId = (Index MaxPECols, Index MaxPERows)

type GlobalAddr = (CoreId, HeapAddr)

-- | Opcodes for primitive operations
data OpCode
  = OpAdd
  | OpSub
  | OpEq
  | OpNeq
  | OpLeq
  | OpUnwrap
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, Enum, Bounded)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| OpCode |]
deriveBitPack [t| OpCode |]

-- | Pointer tags
data PtrTag
  = PUniq
  | PShared
  | PSeq
  | PPar
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)

-- | Single atom
data Atom
  = Fun       FnArity  TemplAddr IsFirst
  -- ^ Template pointer
  | PrimOp    FnArity  IsSwapped OpCode
  -- ^ Primitive operation
  | Ptr       PtrTag HeapAddr
  -- ^ Heap node pointer
  | PrimInt   PInt
  -- ^ Primitive integer literal
  | Con       FnArity  Tag
  -- ^ Constructor tag
  | Arg       PtrTag ArgIndex
  -- ^ Argument pointer
  | Reg       IsShared RegIndex
  -- ^ Primitive register pointer
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)
-- TODO Try making the atom bit pack use one-hot _only_ on stack. Might speed up
-- our control logic?
-- deriveAnnotation defaultDerivator [t| Atom |]
-- deriveBitPack [t| Atom |]
-- TODO The packedDerivator would be more space efficient but it seems broken.
-- Using that we get Fun 0 0 False /= unpack . pack $ Fun 0 0 False...

instance Default Atom where
  def = PrimInt 0

-- | /Inline/ case alternatives
data Alt
  = AFun         TemplAddr
  -- ^ Function pointer (arity can be inferred from the template)
  | AInt FnArity ShortInt
  -- ^ Integer literal
  | ACon FnArity FnArity ShortTag
  -- ^ Constructor tag
  | AArg FnArity ArgIndex
  -- ^ Argument pointer
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)

-- -- TODO This should be generated with TH. Clash's derivePackedAnnotation sounds
-- -- equivalent but it doesn't seem to work for me.
-- {-# ANN module (
--       let { ta = snatToNum (SNat @(BitSize TemplAddr));
--             fa = snatToNum (SNat @(BitSize FnArity));
--             si = snatToNum (SNat @(BitSize ShortInt));
--             st = snatToNum (SNat @(BitSize ShortTag));
--             ai = snatToNum (SNat @(BitSize ArgIndex));
--             msb = pred . fromIntegral $ P.maximum $
--                    P.zipWith (+) [1..] $
--                    P.map P.sum [[ta],[fa, si],[fa, fa, st],[fa, ai]]
--       }
--       in DataReprAnn $(liftQ [t|Alt|]) (msb+1)
--            [ConstrRepr 'AFun (bitmask msb 1) (shiftL 1 (msb-0)) (fieldmasks [ta])
--            ,ConstrRepr 'AInt (bitmask msb 2) (shiftL 1 (msb-1)) (fieldmasks [fa, si])
--            ,ConstrRepr 'ACon (bitmask msb 3) (shiftL 1 (msb-2)) (fieldmasks [fa, fa, st])
--            ,ConstrRepr 'AArg (bitmask msb 4) (shiftL 1 (msb-3)) (fieldmasks [fa, ai])
--            ]) #-}
-- deriveBitPack [t| Alt |]

-- | An expanded verion of `Alt`. Under the hood, the bit representation uses a
--   one-hot encoding for constructor tags, making decode logic a bit faster.
data UnpackedAlt
  = UAFun         TemplAddr
  | UAInt FnArity ShortInt
  | UACon FnArity FnArity ShortTag
  | UAArg FnArity ArgIndex
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| UnpackedAlt |]
deriveBitPack [t| UnpackedAlt |]

-- | Case tables
data CaseTable alt
  = CTInline alt alt
  -- ^ An inline binary choice of alternatives
  | CTOffset TemplAddr
  -- ^ An offset into template memory
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)

-- | Get the primary stack's `PushOffset` from an `Alt`'s pop field.
altPushOffset :: FnArity -> PushOffset
altPushOffset a =
  negate $ unpack
  (resize $ pack a :: BitVector (BitSize PushOffset))

data Update = Update
  { uAddr    :: Maybe HeapAddr
  -- ^ Address to be updated with WHNF
  --   This is a `Maybe` so the GC can nullify the update behaviour
  , uSp      :: Index VStkSize
  -- ^ Pointer to the root of this application on the stack
  , uDiscard :: Bool
  -- ^ Do we want to discard or keep the WHNF on the stack after evaluation?
  -- Performing SEQ needs the discard.
  , uRoot    :: Bool
  -- Is this the update record for the graph root ("main" function)?
  } deriving (Show, ShowX, Generic, NFDataX, BitPack, Eq, Lift)

-- | Heap nodes, indexed by max `App` application length and max `Case` application length
data Node nApp nCase
  = Case (CaseTable Alt) (Len nCase) (Vec nCase (Maybe Atom))
  -- ^ Application describing a case subject with case table for alternatives
  | App  IsNF            (Len nApp ) (Vec nApp  (Maybe Atom))
  -- ^ Plain application
  | Prim RegIndex        (Len 3    ) (Vec  3    (Maybe Atom))
  -- ^ Primitive operation application (for PRS scheme)
  | FetchMe GlobalAddr
  -- ^ A reference to a node stored remotely
  | TSOHead (Maybe GlobalAddr) (Maybe GlobalAddr) HeapAddr -- ^ Continuation of blockee linked list and a reference to the rest of the TSO. Second argument is fetchme if blocked due to a fetch
  | TSOPar (Maybe HeapAddr) (Len nCase) (Vec nCase (Maybe Atom))
  | TSORest (Maybe HeapAddr) (Maybe PInt) (Maybe (CaseTable UnpackedAlt)) (Maybe Update)
  | Locked (Maybe GlobalAddr)
  -- ^ Under evaluation, possibly with another TSO waiting for result
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack, AutoReg)

-- | Node specialised for spinal applications
type SpineNode = Node MaxPush MaxPush
-- | Node specialised for heap applications. N.B. `Case` applications have one
-- less atom, balancing the extra cost of their `CaseTable`.
type HeapNode  = Node NodeLen (NodeLen-1)

-- | An expanded version of `Node`. Makes conditional logic more shallow at the
-- cost of a wider bit representation.
data UnpackedNode len = UnpackedNode
  { nUpdatable :: Bool
  , nArity     :: Len len
  , nAtoms     :: Vec len (Maybe Atom)
  , nCaseTable :: Maybe (CaseTable UnpackedAlt)
  , nRegIndex  :: Maybe RegIndex
  }

-- | A complete template
data Template
  = Template { tPushOffset :: PushOffset
               -- ^ The net effect on stack size after pops and pushes
               -- (combination of template arity and spinal application length).
             , tSpine      :: SpineNode
               -- ^ Spinal application
             , tAps        :: Vec MaxAps (Maybe HeapNode)
               -- ^ Heap applications
             }
  deriving (Eq, Show, Generic, NFDataX, ShowX, BitPack, Lift, AutoReg)

--------------------------------------------------------------------------------
-- Template Helpers

-- | Map a function over `Atom`s in a `Node`.
mapNode :: (KnownNat n, KnownNat m) =>
           (Atom -> Atom) -> Node n m -> Node n m
mapNode f (Case alt arity as)
  = Case alt arity (map (fmap f) as)
mapNode f (App isNF arity as)
  = App isNF arity (map (fmap f) as)
mapNode f (Prim reg arity as)
  = Prim reg arity (map (fmap f) as)
mapNode _ (FetchMe addr) = FetchMe addr
mapNode _ (TSOHead mb hd a) = TSOHead mb hd a
mapNode _ (TSOPar h a as) = TSOPar h a as
mapNode _ (TSORest h p c u) = TSORest h p c u
mapNode _ (Locked t) = Locked t

-- | Set the `Atom`s in a `Node`. For `Case` and `Prim` `Node`s, the `Atom`
-- vector will be truncated.
setAtoms :: HeapNode -> Vec NodeLen (Maybe Atom) -> HeapNode
setAtoms (Case alt arity _) as
  = Case alt arity $ takeI as
setAtoms (App isNF arity _) as
  = App isNF arity as
setAtoms (Prim reg arity _) as
  = Prim reg arity (takeI as)
setAtoms (FetchMe addr) _ = FetchMe addr
setAtoms (TSOHead a hd as) _ = TSOHead a hd as
setAtoms (TSOPar h a as) _ = TSOPar h a as
setAtoms (TSORest h p c u) _ = TSORest h p c u
setAtoms (Locked t) _ = Locked t

getAtom :: (KnownNat n, KnownNat m) =>
           Index (Max n m) -> Node n m -> Maybe Atom
getAtom i (Case _ _ as) = as !! i
getAtom i (App  _ _ as) = as !! i
getAtom i (Prim _ _ as) = as !! i
getAtom _ (FetchMe   _) = Nothing
getAtom _ (TSOHead  {}) = Nothing
getAtom _ (TSOPar   {}) = Nothing
getAtom _ (TSORest  {}) = Nothing
getAtom _ (Locked    _) = Nothing

replaceAtom :: (KnownNat n, KnownNat m) =>
               Maybe Atom -> Index (Max n m) -> Node n m -> Node n m
replaceAtom x i (Case alt arity as)
  = Case alt arity (replace i x as)
replaceAtom x i (App isNF arity as)
  = App isNF arity (replace i x as)
replaceAtom x i (Prim reg arity as)
  = Prim reg arity (replace i x as)
replaceAtom _ _ (FetchMe addr) = FetchMe addr
replaceAtom _ _ (TSOHead mb hd a) = TSOHead mb hd a
replaceAtom _ _ (TSOPar h a as) = TSOPar h a as
replaceAtom _ _ (TSORest h p c u) = TSORest h p c u
replaceAtom _ _ (Locked t) = Locked t

-- | Map a function over `Atom`s in a `Template`.
mapTemplate :: (Atom -> Atom) -> Template -> Template
mapTemplate f (Template po s as)
  = Template po (mapNode f s) (map (fmap (mapNode f)) as)

-- | Unpack a `Node` to an `UnpackedNode`
unpackNode
  :: forall nMax nApp nCase
   . ( KnownNat nMax, KnownNat nApp, KnownNat nCase
     , nApp <= nMax, nCase <= nMax, 3 <= nMax)
  => Bool -> Node nApp nCase -> UnpackedNode nMax
unpackNode shared (Case ct  arity as) =
  UnpackedNode shared (resize arity) (leToPlus @nCase @nMax $ as ++ repeat Nothing) (Just $ unpackCaseTable ct) Nothing
unpackNode shared (App isNF arity as) =
  UnpackedNode (shared && not isNF) (resize arity) (leToPlus @nApp @nMax $ as ++ repeat Nothing) Nothing Nothing
unpackNode shared (Prim reg arity as) =
  UnpackedNode shared (resize arity) (leToPlus @3 @nMax $ as ++ repeat Nothing) Nothing (Just reg)
unpackNode shared (FetchMe _) =
  UnpackedNode shared 0 (repeat Nothing) Nothing Nothing
-- The TSO constructors must return child pointers so GC can detect them.
-- It really shouldn't be used otherwise
unpackNode shared (TSOHead _ _ tl) =
  UnpackedNode shared 0 (leToPlus @1 @nMax $ Just (Ptr PShared tl) :> repeat Nothing) Nothing Nothing
unpackNode shared (TSOPar tl _ as) =
  UnpackedNode shared 0 as' Nothing Nothing
  where
    as' :: Vec nMax (Maybe Atom)
          | isJust tl = y :> ys'
          | otherwise = ys
    ys :: Vec nMax (Maybe Atom)
        = leToPlus @nCase @nMax $ as ++ repeat Nothing
    ys' :: Vec (nMax-1) (Maybe Atom) = init ys
    y = Ptr PShared <$> tl
unpackNode shared (TSORest tl _ _ u) =
  UnpackedNode shared 0 (leToPlus @2 @nMax $ a :> b :> repeat Nothing) Nothing Nothing
  where
    a = tl' <|> u'
    b = const u' =<< tl'
    tl' = Ptr PShared <$> tl
    u'  = Ptr PShared <$> (uAddr =<< u)
    -- Report update addresses as live (when we resume this, we want to know that we're not clobbering another processes new nodes
    -- TODO Still worried if we suspend during GC, are all the update addresses in a good state?
unpackNode shared (Locked _) =
  UnpackedNode shared 0 (repeat Nothing) Nothing Nothing

type Markable a = Maybe (Either a a)
-- ^ Left is an immediate mark, Right is a node that should be traversed

nodeChildren
  :: forall nMax nApp nCase
   . ( KnownNat nMax, KnownNat nApp, KnownNat nCase
     , nApp <= nMax, nCase <= nMax, 3 <= nMax)
  => Node nApp nCase -> Vec nMax (Markable Atom)
nodeChildren (Case _ _ as) =
  leToPlus @nCase @nMax $
    map (fmap Right) as ++ repeat Nothing
nodeChildren (App _ _ as) =
  leToPlus @nApp @nMax $
    map (fmap Right) as ++ repeat Nothing
nodeChildren (Prim _ _ as) =
  leToPlus @3 @nMax $
    map (fmap Right) as ++ repeat Nothing
nodeChildren (FetchMe _) =
  repeat Nothing
nodeChildren (TSOHead _ _ tl) =
  leToPlus @1 @nMax $ Just (Right $ Ptr PShared tl) :> repeat Nothing
nodeChildren (TSOPar tl _ as) = as'
  where
    as' :: Vec nMax (Markable Atom)
          | isJust tl = y :> ys'
          | otherwise = ys
    ys :: Vec nMax (Markable Atom)
        = leToPlus @nCase @nMax $ map (fmap Right) as ++ repeat Nothing
    ys' :: Vec (nMax-1) (Markable Atom) = init ys
    y = Right . Ptr PShared <$> tl
nodeChildren (TSORest tl _ _ u) =
  leToPlus @2 @nMax $ a :> b :> repeat Nothing
  where
    a = tl' <|> u'
    b = const u' =<< tl'
    tl' = Right . Ptr PShared <$> tl
    u' :: Markable Atom
    u'  = Right . Ptr PShared <$> (uAddr =<< u) -- Used to be Left when we were entertaining shallow marking
    -- Report update address as an immediate mark -- don't traverse their subgraphs.
nodeChildren (Locked _) = repeat Nothing

-- | Unpack a `CaseTable Alt` to an `CaseTable UnpackedAlt`
unpackCaseTable :: CaseTable Alt -> CaseTable UnpackedAlt
unpackCaseTable (CTOffset addr) = CTOffset addr
unpackCaseTable (CTInline x y)  = CTInline (unpackAlt x) (unpackAlt y)

-- | Unpack a `Alt` to an `UnpackedAlt`
unpackAlt :: Alt -> UnpackedAlt
unpackAlt (AFun addr)          = UAFun addr
unpackAlt (AInt pop val)       = UAInt pop val
unpackAlt (AArg pop idx)       = UAArg pop idx
unpackAlt (ACon pop arity tag) = UACon pop arity tag

-- | Constructor tag for `False`
falseAtom :: Atom
falseAtom = Con 0 0

-- | Constructor tag for `True`
trueAtom :: Atom
trueAtom = Con 0 1

-- | Translation from Haskell `Bool`s to Heron `Atom` encoding
fromBool :: Bool -> Atom
fromBool True  = trueAtom
fromBool False = falseAtom

-- | Get number of atoms in an application
appLen :: Vec NodeLen (Maybe a) -> Len NodeLen
appLen = fold (+) . map (maybe 0 (const 1))

-- | Checks if a given node is in WHNF
isWHNF :: Node nApp nCase -> Bool
isWHNF (App isNF _ _) = isNF
isWHNF _              = False

-- | Get the arity implied by an `Atom` in normal form. Normal forms can be
--   constructor applications, primitive integers, or partially applied
--   functions. The partially applied functions are handled by underreporting
--   the true arity by one.
atomArity :: Atom -> FnArity
atomArity (PrimInt   _)   = 1
atomArity (Con arity _)   = 1+arity
atomArity (Fun arity _ _) = arity
atomArity _               = 0

-- | Is this `Atom` a `PrimInt`?
isInt :: Atom -> Bool
isInt (PrimInt _) = True
isInt _           = False

-- | Is this `Atom` a `Con`?
isCon :: Atom -> Bool
isCon (Con _ _) = True
isCon _         = False

-- | Is this `PtrTag` something possibly-shared?
isTagShared :: PtrTag -> Bool
isTagShared PUniq = False
isTagShared _     = True

-- | Does this `Atom` point to something possibly-shared?
isShared :: Atom -> Bool
isShared (Fun {})    = False
isShared (PrimOp {}) = False
isShared (Ptr t   _) = isTagShared t
isShared (PrimInt _) = False
isShared (Con {})    = False
isShared (Arg t _)   = isTagShared t
isShared (Reg s _)   = s

-- | Maybe return a `HeapAddr` pointed to by this `Atom`
heapAddr :: Atom -> Maybe HeapAddr
heapAddr (Ptr _ a) = Just a
heapAddr _         = Nothing

-- | Sets the head address of any `Ptr` while maintaining its flags
setHeapAddr :: HeapAddr -> Atom -> Atom
setHeapAddr addr (Ptr mode _) = Ptr mode addr
setHeapAddr _ a               = a

-- | Addition for `Index` using raw bits. Our relative `Ptr` addresses might be
-- negative and the raw interpretation is needed to avoid `Index` bound checks.
rawAdd :: (KnownNat n, 1 <= n)
       => Index n -> Index n -> Index n
rawAdd x y = unpack $ pack x + pack y

-- | Can we perform GC with this `Atom` at the top of the stack? We should not
-- initiate a garbage collection event while instantiating split templates.
canGC :: Atom -> Bool
canGC (Fun _ _ False) = False
canGC (Ptr _ _)       = False -- If we pause for whatever reason while unwinding, the scheduler might have placed a lock on it...
canGC _               = True

-- | Mark this `Atom` as possibly-shared
dash :: Atom -> Atom
dash (Ptr PUniq addr)  = Ptr PShared addr
dash (Arg PUniq index) = Arg PShared index
dash (Reg _ index)     = Reg True index
dash a                 = a

-- | Conditionally mark this `Atom` as possibly-shared
dashIf :: Bool -> Atom -> Atom
dashIf True  = dash
dashIf False = id

forcePtrTag :: PtrTag -> Atom -> Atom
forcePtrTag PSeq (Ptr _ addr) = Ptr PSeq addr
forcePtrTag PPar (Ptr _ addr) = Ptr PPar addr
forcePtrTag _ _               = Fun 1 1 True -- Resolves to the `id` function. Used as a hack
                               -- for ignoring seq/pars on arguments who turn
                               -- out to not be heap references.

blockee :: Node n m -> Maybe GlobalAddr
blockee (Locked b) = b
blockee _          = Nothing

isLocked :: Node n m -> Bool
isLocked (Locked _) = True
isLocked _          = False

--------------------------------------------------------------------------------
-- Template Pretty Printing

ppVec :: Show a => String -> (a -> String) -> Vec n a -> String
ppVec ind f = foldl (\str a -> str P.++ ind P.++ f a P.++ "\n") ""

-- | Pretty print a `Node`
ppNode :: (KnownNat n, KnownNat m) => String -> Node n m -> String
ppNode ind (Case alt arity as)
  = P.unlines (P.map (ind P.++)
                 [ "Case to " P.++ show alt
                 , "  Arity = " P.++ show arity
                 , "  Atoms ->"
                 ]) P.++ ppVec ("           " P.++ ind) show as
ppNode ind (App isNF arity as)
  = P.unlines (P.map (ind P.++)
                 [ "App " P.++ (if isNF then "NF" else "Thunk")
                 , "  Arity = " P.++ show arity
                 , "  Atoms ->"
                 ]) P.++ ppVec ("           " P.++ ind) show as
ppNode ind (Prim reg arity as)
  = P.unlines (P.map (ind P.++)
                 [ "PrimRedex to " P.++ show reg
                 , "  Arity = " P.++ show arity
                 , "  Atoms ->"
                 ]) P.++ ppVec ("           " P.++ ind) show as
ppNode ind (FetchMe ga)
  = P.unlines [ind P.++ ("FetchMe from " P.++ show ga)]
ppNode ind (TSOHead mb hd a)
  = P.unlines (P.map (ind P.++)
                 [ "TSO Header "
                 , "  Next blockee -> " P.++ show mb
                 , "  Head node    -> " P.++ show hd
                 , "  Body         -> " P.++ show a
                 ])
ppNode ind (TSOPar h a as)
  = P.unlines (P.map (ind P.++)
                 [ "TSO Stack "
                 , "  Tail -> " P.++ show h
                 , "  Arity = " P.++ show a
                 , "  Atoms -> "
                 ]) P.++ ppVec ("           " P.++ ind) show as
ppNode ind (TSORest h p c u)
  = P.unlines (P.map (ind P.++)
                 [ "TSO Stack "
                 , "  Tail -> " P.++ show h
                 , "  Prim -> " P.++ show p
                 , "  Alt  -> " P.++ show c
                 , "  Upd  -> " P.++ show u
                 ])
ppNode ind (Locked t)
  = ind P.++ "Locked " P.++ show t

-- | Pretty print a `Maybe Node`
ppNodeM :: (KnownNat n, KnownNat m) => String -> Maybe (Node n m) -> String
ppNodeM ind (Just n) = ppNode ind n
ppNodeM _ Nothing    = ""

-- | Pretty print a `Template`
ppTemplate :: String -> Template -> String
ppTemplate ind t
  = P.unlines $ P.map (ind P.++)
      [ "Template ->"
      , "  Offset" P.++ show (tPushOffset t)
      , "  SpineAp ->\n" P.++ ppNode (ind P.++ "             ")
                                     (tSpine t)
      , "  HeapAps ->\n" P.++ ppVec  (ind P.++ "             ")
                                     (ppNodeM (ind P.++ "             "))
                                     (tAps t)
      ]

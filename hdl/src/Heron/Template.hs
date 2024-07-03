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
  , PInt
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
  , AtomNode
  , SpineNode
  , HeapNode
  -- ** Atoms
  , Atom(..)
  -- ** Case handling
  , Alt(..)
  , CaseTable(..)

  -- * Unpacked Representations
  , UnpackedNode(..)
  , unpackNode
  , UnpackedAlt(..)
  , unpackAlt
  , unpackCaseTable

  -- * Helpers
  -- ** Booleans
  , falseAtom
  , trueAtom
  , fromBool

  -- ** Inspections
  , isInt
  , isCon
  , isShared
  , heapAddr
  , atomArity
  , appLen
  , rawAdd
  , canGC
  , altPushOffset

  -- ** Mutations
  , mapNode
  , mapTemplate
  , dash
  , dashIf

  -- * Pretty printing
  , ppNode
  , ppNodeM
  , ppTemplate

  ) where

import           Clash.Annotations.BitRepresentation.Deriving
import           Clash.Prelude                                hiding (msb)
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
-- | Primitive integers
type PInt       = Signed   IntW
-- | Short primitive integers (for inline case tables)
type ShortInt   = Signed   ShortIntW
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
type IsSwapped   = Bool
-- | Is this node possibly shared (and will need updating with its normal
-- form)?
type IsShared    = Bool
-- | Is this node already in its normal form? TODO Is storing this any better than
type IsNF        = Bool
-- | Is this template the first in a series of split templates?
type IsFirst     = Bool

-- | Encodes natural numbers from 0 to n, inclusive.
type Len n = Index (1+n)

-- | Opcodes for primitive operations
data OpCode
  = OpAdd
  | OpSub
  | OpEq
  | OpNeq
  | OpLeq
  | OpSeq
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, Enum, Bounded)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| OpCode |]
deriveBitPack [t| OpCode |]

-- | Single atom
data Atom
  = Fun       FnArity  TemplAddr IsFirst
  -- ^ Template pointer
  | PrimOp    FnArity  IsSwapped OpCode
  -- ^ Primitive operation
  | Ptr       IsShared HeapAddr
  -- ^ Heap node pointer
  | PrimInt   PInt
  -- ^ Primitive integer literal
  | Con       FnArity  Tag
  -- ^ Constructor tag
  | Arg       IsShared ArgIndex
  -- ^ Argument pointer
  | Reg       IsShared RegIndex
  -- ^ Primitive register pointer
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)
-- TODO Try making the atom bit pack use one-hot _only_ on stack. Might speed up
-- our control logic?

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

-- | Heap nodes, indexed by max `App` application length and max `Case` application length
data Node nApp nCase
  = Case (CaseTable Alt) (Len nCase) (Vec nCase (Maybe Atom))
  -- ^ Application describing a case subject with case table for alternatives
  | App  IsNF            (Len nApp ) (Vec nApp  (Maybe Atom))
  -- ^ Plain application
  | Prim RegIndex        (Len 3    ) (Vec  3    (Maybe Atom))
  -- ^ Primitive operation application (for PRS scheme)
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift, BitPack)

-- | Node specialised for single atoms
type AtomNode  = Node 1       1
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
  deriving (Eq, Show, Generic, NFDataX, ShowX, BitPack, Lift)

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

-- | Map a function over `Atom`s in a `Template`.
mapTemplate :: (Atom -> Atom) -> Template -> Template
mapTemplate f (Template po s as)
  = Template po (mapNode f s) (map (fmap (mapNode f)) as)

-- | Unpack a `Node` to an `UnpackedNode`
unpackNode
  :: forall nMax nApp nCase
   . ( KnownNat nApp, KnownNat nCase, KnownNat nMax
     , nApp <= nMax, nCase <= nMax, 3 <= nMax )
  => Bool -> Node nApp nCase -> UnpackedNode nMax
unpackNode shared (Case ct  arity as) =
  UnpackedNode shared (resize arity) (leToPlus @nCase @nMax $ as ++ repeat Nothing) (Just $ unpackCaseTable ct) Nothing
unpackNode shared (App isNF arity as) =
  UnpackedNode (shared && not isNF) (resize arity) (leToPlus @nApp @nMax $ as ++ repeat Nothing) Nothing Nothing
unpackNode shared (Prim reg arity as) =
  UnpackedNode shared (resize arity) (leToPlus @3 @nMax $ as ++ repeat Nothing) Nothing (Just reg)

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
appLen :: Vec NodeLen (Maybe Atom) -> Len NodeLen
appLen = fold (+) . map (maybe 0 (const 1))

-- | Get the arity implied by an `Atom`
atomArity :: Atom -> FnArity
atomArity (Fun arity _ _)    = arity
atomArity (PrimOp arity _ _) = arity
atomArity (PrimInt   _)      = 1
atomArity (Con arity _)      = 1+arity
atomArity _                  = 0

-- | Is this `Atom` a `PrimInt`?
isInt :: Atom -> Bool
isInt (PrimInt _) = True
isInt _           = False

-- | Is this `Atom` a `Con`?
isCon :: Atom -> Bool
isCon (Con _ _) = True
isCon _         = False

-- | Does this `Atom` point to something possibly-shared?
isShared :: Atom -> Bool
isShared (Fun {})      = False
isShared (PrimOp {})   = False
isShared (Ptr sh _)    = sh
isShared (PrimInt   _) = False
isShared (Con {})      = False
isShared (Arg s _)     = s
isShared (Reg s _)     = s

-- | Maybe return a `HeapAddr` pointed to by this `Atom`
heapAddr :: Atom -> Maybe HeapAddr
heapAddr (Ptr _    a) = Just a
heapAddr _            = Nothing

-- | Addition for `Index` using raw bits. Our relative `Ptr` addresses might be
-- negative and the raw interpretation is needed to avoid `Index` bound checks.
rawAdd :: (KnownNat n, 1 <= n)
       => Index n -> Index n -> Index n
rawAdd x y = unpack $ pack x + pack y

-- | Can we perform GC with this `Atom` at the top of the stack? We should not
-- initiate a garbage collection event while instantiating split templates.
canGC :: Atom -> Bool
canGC (Fun _ _ False) = False
canGC _               = True

-- | Mark this `Atom` as possibly-shared
dash :: Atom -> Atom
dash (Ptr _ addr)  = Ptr True addr
dash (Arg _ index) = Arg True index
dash (Reg _ index) = Reg True index
dash a             = a

-- | Conditionally mark this `Atom` as possibly-shared
dashIf :: Bool -> Atom -> Atom
dashIf True  = dash
dashIf False = id

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

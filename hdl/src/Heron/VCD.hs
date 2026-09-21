-- | Module to parse dumped VCD files back into our own datatypes, preserving
-- undefined values. The parser is a modified version of
-- https://hackage.haskell.org/package/vcd, supporting undefined bits.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Heron.VCD
  ( TraceState(..)
  , unpackBitStr
  , unpackMask
  , vcdToStream
  ) where

import qualified Clash.Prelude                  as C
import qualified Data.ByteString.Lazy.Char8     as B

import           Clash.Sized.Internal.BitVector (BitVector (BV))
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           GHC.Natural                    (Natural)
import           Prelude
import           System.Process                 (callCommand)
-- import           Text.ParserCombinators.Poly.Lazy

-- | VCD Timescale.
data Timescale
  = S     -- ^ seconds
  | MS    -- ^ milliseconds
  | US    -- ^ microseconds
  | PS    -- ^ picoseconds

instance Show Timescale where
  show S  = "s"
  show MS = "ms"
  show US = "us"
  show PS = "ps"

-- | VCD database.
data VCD = VCD Timescale [Definition] [(Int, [(String, Value)])] deriving Show

-- | Recorded value.
type Value = [C.Bit]

dontcare :: C.Bit
dontcare = C.unpack $(C.bLit ".")

-- | Variable definition.
data Definition
  = Scope String [Definition]     -- ^ Hierarchical scope.
  | Var String Int String String  -- ^ Variable with type, width, code, name.
  deriving Show

-- A more fragile version of the parser that generates a VCD structure lazily
parseVCDLazy :: FilePath -> IO VCD
parseVCDLazy f = VCD PS <$> defs <*> samples
  where
    defs = do
      callCommand $ unwords ["grep \"^\\$var\"", f, "> /tmp/vcddefs"]
      parseDefs <$> B.readFile "/tmp/vcddefs"
    samples = do
      callCommand $ unwords ["grep \"^#\\|^b\"", f, "> /tmp/vcdvals"]
      parseSamples <$> B.readFile "/tmp/vcdvals"

    parseDefs = map (parseDef . B.words) . B.lines
    parseDef [_,ty,width,code,name,_] = Var (B.unpack ty) (read $ B.unpack width) (B.unpack code) (B.unpack name)
    parseDef s = error $ "Parse of def has wrong number of lines: " ++ show s

    parseSamples = reverse . consumeSamples [] . map B.words . B.lines
    consumeSamples :: [(Int, [(String, Value)])] -> [[B.ByteString]] -> [(Int, [(String, Value)])]
    consumeSamples acc [] = acc
    consumeSamples acc ([t]:rest) = consumeSamples ((read $ B.unpack $ B.tail t, map parseVal vals) : acc) tl
      where
        (vals,tl) = break ((=='#') . B.head . head) rest
    consumeSamples _ _ = error "Parse of sample values is bad"
    parseVal [bits, code] = (B.unpack code, map toBit $ B.unpack $ B.tail bits)
    parseVal s                = error $ "Parse of sample value is bad: " ++ B.unpack (B.unwords s)

{-
-- This original version of the parser had HUGE space leaks
data Token
  = End
  | Timescale
  | Scope'
  | Var'
  | UpScope
  | EndDefinitions
  | DumpVars
  | Step Int
  | String String
  deriving (Show, Eq)

type VCDParser = Parser Token

-- | Parse VCD data.
parseVCD :: String -> VCD
parseVCD = fst . runParser vcd . dropWhile (/=Timescale) . map token . words
  where
    token :: String -> Token
    token a = case a of
      "$end"                                -> End
      "$timescale"                          -> Timescale
      "$scope"                              -> Scope'
      "$var"                                -> Var'
      "$upscope"                            -> UpScope
      "$enddefinitions"                     -> EndDefinitions
      "$dumpvars"                           -> DumpVars
      '#':i | not (null i) && all isDigit i -> Step $ read i
      _                                     -> String a


tok :: Token -> VCDParser ()
tok a = void (satisfy (== a))

str :: VCDParser String
str = do
  String sc <- satisfy (\case { String _ -> True; _ -> False })
  return sc

vcd :: VCDParser VCD
vcd = return f
  `apply`   timescale
  `apply`   definitions
  `discard` tok EndDefinitions
  `discard` tok End
  `apply` step_
  `discard` tok DumpVars
  `apply`   values
  `discard` tok End
  `apply`   many sample
  `discard` eof
  where
    f ts defs initTime initValues samples =
      VCD ts defs $ (initTime, initValues) : samples

timescale :: VCDParser Timescale
timescale = do
  tok Timescale
  sc <- str
  tok End
  case sc of
    "1s"  -> return S
    "1ms" -> return MS
    "1us" -> return US
    "1ps" -> return PS
    _     -> error $ "invalid timescale: " ++ sc

definitions :: VCDParser [Definition]
definitions = many $ oneOf [scope_, var_]

scope_ :: VCDParser Definition
scope_ = do
  tok Scope'
  _ <- str
  name <- str
  tok End
  defs <- definitions
  tok UpScope
  tok End
  return $ Scope name defs

var_ :: VCDParser Definition
var_ = do
  tok Var'
  typ   <- str
  width <- str
  code  <- str
  name  <- str
  tok End
  return $ Var typ (read width) code name

step_ :: VCDParser Int
step_ = do
  Step a <- satisfy (\case { Step _ -> True; _ -> False })
  return a

sample :: VCDParser (Int, [(String, Value)])
sample = do
  i <- step_
  a <- values
  return (i, a)

values :: VCDParser [(String, Value)]
values = many value

value :: VCDParser (String, Value)
value = do
  s <- str
  case s of
    ('0':code) -> return (code, [C.low] )
    ('1':code) -> return (code, [C.high])
    ('b':bits) -> do code <- str
                     return (code, map toBit bits)
    a          -> error $ "invalid value: " ++ a
-}

toBit :: Char -> C.Bit
toBit '0' = C.low
toBit '1' = C.high
toBit 'x' = dontcare
toBit c   = error $ "invalid bit: " ++ show c

class TraceState a where
  keys :: [String]
  recreate :: [[C.Bit]] -> a
  recreateBV :: [(Natural,Natural)] -> a

unpackBitStr
  :: forall a
   . C.BitPack a
  => [C.Bit] -> a
unpackBitStr = C.unpack . foldr (C.+>>.) (0 :: C.BitVector (C.BitSize a))

unpackMask
  :: forall a
   . C.BitPack a
  => (Natural, Natural) -> a
unpackMask (m,v) = C.unpack (BV m v :: C.BitVector (C.BitSize a))

rebuild :: forall a . TraceState a => VCD -> [a]
rebuild (VCD _ defs ((0,is):xs)) = go starts 0 xs
  where
    hasName n (Var _ _ _ m) = n==m
    hasName _ _             = False
    getCode (Var _ _ c _) = c
    getCode (Scope _ _  ) = error "No codename for a scope"

    extDefs (Scope _ ds) = ds
    extDefs d            = [d]
    flatDefs = concatMap extDefs defs

    codes  = map (\n -> getCode . head $ filter (hasName n) flatDefs) (keys @a)
    starts = map (fromJust . flip lookup is) codes

    go :: [[C.Bit]] -> Int -> [(Int,[(String, Value)])] -> [a]
    go _  _ [] = []
    go st n ((m,s):ss)
      | n < m = recreate st : go st (n+1) ((m,s):ss)
      | otherwise = let st' = zipWith (\i x -> fromMaybe (st !! i) $ lookup x s) [0..] codes
                    in recreate st' : go st' (n+1) ss
rebuild _ = error "Got a VCD structure with no initial states"

vcdToStream :: forall a . TraceState a => String -> IO [a]
vcdToStream f = parseVCDLazy f <&> rebuild

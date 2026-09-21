module Flite.Parse where

import Flite.Syntax
import Flite.Pretty
import Flite.Traversals
import Data.Functor ((<$),(<&>))

-- import Control.Applicative
import           Control.Arrow                       hiding (app)
import           Control.Monad
-- import Control.Monad.State
import Data.Char hiding (chr)
import Data.List
import Data.Maybe (catMaybes)
import Text.Parsec.Expr
import Text.Parsec.Indent
import qualified Text.Parsec.Indent.Explicit as IE
import Text.Parsec
import Text.Parsec.Pos (SourcePos)
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Functor.Identity
import Paths_flite

type Parser a = ParsecT String () (IndentT Identity) a

flite :: T.GenTokenParser String () (IndentT Identity)
flite = T.makeTokenParser fliteDef

fliteDef = T.LanguageDef
      { T.commentStart   = "{-"
      , T.commentEnd     = "-}"
      , T.commentLine    = "--"
      , T.nestedComments = True
      , T.identStart     = letter
      , T.identLetter    = alphaNum <|> oneOf "_'"
      , T.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , T.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , T.reservedOpNames= ["="
                           ,"\\"
                           ,"->"
                           ,"++"
                           ,"-","+","==","/=","<=",">","<",">="
                           ,"&&", "||"
                           ,".", ".."
                           ,"|", "!!"
                           ]
      , T.reservedNames  = ["let"
                           ,"in"
                           ,"case"
                           ,"of"
                           ,"if"
                           ,"then"
                           ,"else"
                           ,"where"
                           ]
      , T.caseSensitive  = True
      }

identifier    = T.identifier flite
reservedOp    = T.reservedOp flite
reserved      = T.reserved flite
natural       = T.natural flite
parens        = T.parens flite
semi          = T.semi flite
braces        = T.braces flite
brackets      = T.brackets flite
symbol        = T.symbol flite
operator      = T.operator flite
charLiteral   = T.charLiteral flite
stringLiteral = T.stringLiteral flite
whiteSpace    = T.whiteSpace flite
comma         = T.comma flite

prog :: Parser Prog
prog = catMaybes <$> (block (whiteSpace *> def) <* eof)
  where
    def = (Nothing <$ try typeDef) <|> try (Just <$> defn)

typeDef :: Parser ()
typeDef = withPos (do
  _    <- symbol "data" <|> symbol "type"
  f    <- try conId
  args <- many pat
  reservedOp "="
  sameOrIndented
  body <- expr
  pure ()
  ) <?> "type def"

tryFor s m = try m <?> s

binApp t x y = App t [x, y]
consOrVar n = if isLower (head n) then Var n else Con n
infixName = Infix (try (symbol "`" *> identifier <* symbol "`") <&> (binApp . consOrVar) ) AssocLeft
infixSeq = Infix (try (symbol "`" *> symbol "seq" <* symbol "`") <&> (binApp . consOrVar) ) AssocRight
infixPar = Infix (try (symbol "`" *> symbol "par" <* symbol "`") <&> (binApp . consOrVar) ) AssocRight

binaryOp op = Infix (reservedOp op >> return (binApp (Fun $ "(" ++ op ++ ")")))
listCons = Infix (symbol ":" >> return (binApp (Con "Cons"))) AssocRight
dollarOp = Infix (symbol "$" >> return (\x y -> App x [y]))   AssocRight

opTable = [ [binaryOp "."  AssocRight                                                                           ]
          , [binaryOp "+"  AssocLeft , binaryOp "-"  AssocLeft , binaryOp "!!"  AssocLeft                       ]
          , [binaryOp "==" AssocNone , binaryOp "/=" AssocNone , binaryOp "<=" AssocNone, binaryOp ">" AssocNone
                                                               , binaryOp ">=" AssocNone, binaryOp "<" AssocNone]
          , [binaryOp "&&" AssocRight, binaryOp "||" AssocRight                                                 ]
          , [listCons                , binaryOp "++" AssocRight                                                 ]
          , [infixSeq, infixPar, infixName                                                                      ]
          , [dollarOp                                                                                           ]
          , [binaryOp "|"  AssocLeft                                                                            ]
            -- ^ This is only used to discard type definitions
          ]

-- | Constructor names
conId :: Parser Id
conId = tryFor "constructor" $
            try ("Nil"  <$ symbol "[]"  )
        <|> try ("Pair" <$ symbol "(,)" )
        <|> try ("Cons" <$ symbol "(:)" )
        <|> (do c <- identifier
                if isUpper (head c)
                    then return c
                    else unexpected ("variable " ++ show c)
            )

-- | Constructors
con :: Parser Exp
con = Con <$> conId

-- | Variable names
varId :: Parser Id
varId = tryFor "variable" $ do
    v <- identifier
    if isLower (head v) || head v == '_'
        then return v
        else unexpected ("constructor " ++ show v)

-- | Variables
var = Var <$> varId

-- | Primitive op names
primId :: Parser Id
primId = tryFor "primitive op" $ do
  symbol "("
  v <- choice $ map (\f -> try (reservedOp f) >> pure (wrap f)) primitives
  symbol ")"
  pure v
  where
    primitives = ["++", "+", "-", "==", "/=", "<=", "&&", "||", ">", ">=", "<", ".", "$", "!!"]
    wrap f = "(" ++ f ++ ")"

-- | Primitive op
prim :: Parser Exp
prim = Fun <$> primId

-- | Function definitions
defn :: Parser Decl
defn = withPos (do
  f    <- try varId <|> try primId
  args <- many pat
  reservedOp "="
  sameOrIndented
  Func f args <$> expr
  ) <?> "definition"

pat :: Parser Exp
pat =   try con
    <|> try var
    <|> try wild
    <|> try pair
    <|> try list
    <|> parens expr
    <?> "pattern"

app :: [Exp] -> Exp
app []       = error "Flite.Parse.app: Got an empty list of expressions"
app [f]      = f
app (f:args) = App f args

expr :: Parser Exp
expr = buildExpressionParser opTable $ withPos
       (pure app <*/> atom <?> "expr")

-- TODO Trying to fix ifThenElse parsing for braun example.

atom :: Parser Exp
atom =   case_
     <|> let_
     -- <|> lam --TODO
     <|> ifThenElse
     <|> int
     <|> chr
     <|> prim
     <|> str
     <|> var
     <|> con
     <|> listEnums
     <|> list
     <|> pair
     <|> parens expr
     <?> "expression"

case_ :: Parser Exp
case_ = withPos $ do
  reserved "case"
  subj <- expr
  reserved "of"
  alts <- block alt
  return $ Case subj alts
  where
    alt = withPos $ do
      c <- many pat
      reservedOp "->"
      indented
      rhs <- expr
      return (app c, rhs)

let_ :: Parser Exp
let_ = do
  reserved "let"
  bs <- block bind
  reserved "in"
  Let bs <$> expr
  where
    bind = ((,) <$> varId) <*> (reservedOp "=" *> expr)

list = tryFor "list" $ do
  xs <- brackets (expr `sepBy1` comma)
  pure $ foldr (\x y -> App (Con "Cons") [x,y]) (Con "Nil") xs

listEnums =
      listEnumFromThen
  <|> listEnumFromThenTo
  <|> listEnumFrom
  <|> listEnumFromTo

listEnumFrom = tryFor "listEnumFrom" $ do
  n <- brackets (expr <*  reservedOp "..")
  pure $ App (Fun "enumFrom") [n]

listEnumFromThen = tryFor "listEnumFromThen" $ do
  symbol "["
  n <- expr
  comma
  n' <- expr
  reservedOp ".."
  symbol "]"
  pure $ App (Fun "enumFromThen") [n,n']

listEnumFromTo = tryFor "listEnumFromTo" $ do
  symbol "["
  n <- expr
  reservedOp ".."
  m <- expr
  symbol "]"
  pure $ App (Fun "enumFromTo") [n,m]

listEnumFromThenTo = tryFor "listEnumFromThenTo" $ do
  symbol "["
  n <- expr
  comma
  n' <- expr
  reservedOp ".."
  m <- expr
  symbol "]"
  pure $ App (Fun "enumFromThenTo") [n,n',m]

pair = tryFor "pair" $ do
  symbol "("
  a <- expr
  comma
  b <- expr
  symbol ")"
  pure $ App (Con "Pair") [a,b]

lam :: Parser Exp
lam = undefined

ifThenElse :: Parser Exp
ifThenElse = tryFor "ifThenElse" $ do
  reserved "if"
  scr <- expr
  reserved "then"
  t   <- expr
  reserved "else"
  f   <- expr
  pure $ Case scr [(Con "True", t), (Con "False", f)]

int :: Parser Exp
int = Int . fromInteger <$> natural

chr :: Parser Exp
chr = Int . ord <$> charLiteral

wild :: Parser Exp
wild = Wld <$ reserved "_"

str :: Parser Exp
str = stringExp <$> stringLiteral
  where
  stringExp []     = App (Con "Nil" ) []
  stringExp (x:xs) = App (Con "Cons") [Int . ord $ x, stringExp xs]

testParser :: Parser a -> String -> Either ParseError a
testParser p = runIndent . runParserT p () "test"

supplyPrelude :: Prog -> String -> Prog
supplyPrelude p prelude = foldr addFunc p prelude'
  where
    prelude' =
      let res = runIndent $ runParserT prog () "prelude" prelude
      in case res of
        Left e  -> error . show$ e
        Right p -> p
    fns = map funcName p
    addFunc d@(Func f _ _) p | f `elem` fns = error $ "Source overrides prelude function: " ++ f
                             | otherwise = d : p

parseProgFile :: SourceName -> IO Prog
parseProgFile f = do
  src <- readFile f
  preludeFile <- getDataFileName "data/Prelude.fl"
  let res = runIndent $ runParserT prog () f src
  case res of
    Left e  -> error . show$ e
    Right p -> supplyPrelude p <$> readFile preludeFile

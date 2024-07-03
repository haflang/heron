module Flite.Parse where
import qualified Flite.Prelude as Prelude
import Flite.Syntax
import Flite.Pretty

-- import Control.Applicative
import Control.Arrow hiding (app)
import Control.Monad
-- import Control.Monad.State
import Data.Char hiding (chr)
import Data.List
import Text.Parsec.Expr
import Text.Parsec.Indent
import qualified Text.Parsec.Indent.Explicit as IE
import Text.Parsec
import Text.Parsec.Pos (SourcePos)
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Functor.Identity

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
                           ,"-","+","==","/=","<="
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
prog = block (whiteSpace *> defn) <* eof

tryFor s m = try m <?> s

binApp t x y = App t [x, y]
consOrVar n = if isLower (head n) then Var n else Con n
infixName = Infix (try (symbol "`" *> identifier <* symbol "`") >>= return . binApp . consOrVar ) AssocLeft

binaryOp op assoc = Infix (reservedOp op >> return (binApp (Fun $ "(" ++ op ++ ")"))) assoc
listCons = Infix (symbol ":" >> return (binApp (Con "Cons"))) AssocRight
dollarOp = Infix (symbol "$" >> return (\x y -> App x [y]))   AssocRight

opTable = [ [infixName              , binaryOp "."  AssocRight                        ]
          , [binaryOp "+" AssocLeft , binaryOp "-"  AssocLeft                         ]
          , [binaryOp "==" AssocNone, binaryOp "/=" AssocNone, binaryOp "<=" AssocNone]
          , [listCons               , dollarOp                                        ]
          ]

-- | Constructor names
conId :: Parser Id
conId = tryFor "constructor" $
            try (pure "Nil"   <* symbol "[]"  )
        <|> try (pure "Pair"  <* symbol "(,)" )
        <|> try (pure "Cons"  <* symbol "(:)" )
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
    primitives = ["+", "-", "==", "/=", "<="]
    wrap f = "(" ++ f ++ ")"

-- | Primitive op
prim :: Parser Exp
prim = Fun <$> primId

-- | Function definitions
defn :: Parser Decl
defn = withPos (do
  f    <- varId
  args <- many pat
  reservedOp "="
  sameOrIndented
  body <- expr
  pure $ Func f args body
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
app [f] = f
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
  scope <- expr
  return $ Let bs scope
  where
    bind = pure (,) <*> varId <*> (reservedOp "=" *> expr)

list = tryFor "list" $ do
  xs <- brackets (expr `sepBy1` comma)
  pure $ foldr (\x y -> App (Con "Cons") [x,y]) (Con "Nil") xs

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
int = pure Int <*> (pure fromInteger <*> natural)

chr :: Parser Exp
chr = pure (Int . ord) <*> charLiteral

wild :: Parser Exp
wild = pure Wld <* reserved "_"

str :: Parser Exp
str = pure stringExp <*> stringLiteral
  where
  stringExp []     = App (Con "Nil" ) []
  stringExp (x:xs) = App (Con "Cons") [Int . ord $ x, stringExp xs]

{-
Tuples:
tms <- parens (term `sepBy1` comma)
        return $ foldl (\x y -> Pair x y) (head tms) (tail tms)
Something similar for lists
-}
testParser :: Parser a -> String -> Either ParseError a
testParser p = runIndent . runParserT p () "test"

parseProgFile :: SourceName -> IO Prog
parseProgFile f = do
  src <- readFile f
  let res = runIndent $ runParserT prog () f src
  case res of
    Left e  -> error . show$ e
    Right p -> return . Prelude.supplyPrelude $ p

{-
TODO

Handle tuples

-}

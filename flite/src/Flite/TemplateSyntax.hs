module Flite.TemplateSyntax where

type Id = Int

type Arity = Int

type Index = Int

type Shared = Bool

type Seq = Bool

type Par = Bool

data Atom =
    INT Int
  | ARG Shared Seq Par Int
  | VAR Shared Seq Par Int
  | REG Shared Int
  | CON Arity Index
  | FUN Bool Arity Id
  | PRI Arity String
  deriving (Show, Read, Eq)

type Normal = Bool

type RegId = Int

data App = APP Normal [Atom] | CASE LUT [Atom] | PRIM RegId [Atom]
  deriving (Show, Read, Eq)

data LUT = LOffset Int
         | LInline [(Int, Atom)]
  deriving (Show, Read, Eq)


type Template = (String, Int, Maybe Int, [LUT], [Atom], [App])

type Prog = [Template]

readProg :: FilePath -> IO Prog
readProg f
  = do templs <- lines <$> readFile f
       pure $ map read templs

appLen :: App -> Int
appLen (APP  _ as) = length as
appLen (CASE _ as) = length as
appLen (PRIM _ as) = length as

appAtoms :: App -> [Atom]
appAtoms (APP  _ as) = as
appAtoms (CASE _ as) = as
appAtoms (PRIM _ as) = as

isFUN :: Atom -> Bool
isFUN (FUN {}) = True
isFUN        _ = False

isVAR :: Atom -> Bool
isVAR (VAR {}) = True
isVAR       _  = False

-- Pretty print

pretty :: Int -> Template -> String
pretty i (f, a, s, alts, stk, aps) = unlines $ lhs : body ++ ct ++ [""]
  where
    ind  = "  "
    lhs  = f ++ " @" ++ show i ++ " |" ++ show a ++ "|" ++
           maybe "" (\x -> " (spark @"++ show x ++ ")") s ++ " = "
    ct   | null alts = []
         | otherwise
         = [ind ++ " of <" ++ show alts ++ ">"]
    body | null aps
         = [ind ++ show stk]
         | otherwise
         = zipWith (++)
             (           (ind ++ "let 0 -> ") :
               map (\n -> ind ++ "    " ++ show n ++ " -> ")
                   [1 :: Integer ..]
             )
             (map show aps)
           ++ [ind ++ " in " ++ show stk]

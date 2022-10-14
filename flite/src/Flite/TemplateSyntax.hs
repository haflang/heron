module Flite.TemplateSyntax where

type Id = Int

type Arity = Int

type Index = Int

type Shared = Bool

data Atom =
    INT Int
  | ARG Shared Int
  | VAR Shared Int
  | REG Shared Int
  | CON Arity Index
  | FUN Bool Arity Id
  | PRI Arity String
  deriving (Show, Read)

type Normal = Bool

type RegId = Int

data App = APP Normal [Atom] | CASE LUT [Atom] | PRIM RegId [Atom]
  deriving (Show, Read)

type LUT = Int

type Template = (String, Int, [LUT], [Atom], [App])

type Prog = [Template]

readProg :: FilePath -> IO Prog
readProg f
  = do templs <- lines <$> readFile f
       pure $ map read templs

appLen :: App -> Int
appLen (APP  _ as) = length as
appLen (CASE _ as) = length as
appLen (PRIM _ as) = length as

isFUN :: Atom -> Bool
isFUN (FUN _ _ _) = True
isFUN           _ = False

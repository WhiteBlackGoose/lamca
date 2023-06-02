module Parser (
  pAlpha
, pLambda
, pBraces
, pExpr
) where

import Text.Megaparsec (Parsec, some, single)

import Types (VarName, Expression (Variable, Application, Abstraction))
import qualified Text.ParserCombinators.ReadPrec
import Text.Read (Read(readPrec))
import Data.Void (Void)
import Text.Megaparsec.Byte (char, alphaNumChar)
import Text.Megaparsec (choice)

type Parser = Parsec Void String

data Result =
  Valid Expression
  | Error
  deriving (Show)

rmap f (Valid e) = Valid (f e)
rmap _ err = err

pAlpha :: Parser VarName
pAlpha = choice $ map single ['a'..'z']

pLambda :: Parser (Expression -> Expression)
pLambda = do
  _ <- single '|'
  vs <- some pAlpha
  _ <- single '.'
  return $ foldl (.) id (map Abstraction vs)

pBraces :: Parser Expression
pBraces = do
  _ <- single '('
  expr <- pExpr
  _ <- single ')'
  return expr

pExpr :: Parser Expression
pExpr = do
  f:st <- some $ choice [
    pBraces
    , pLambda <*> pExpr
    , Variable <$> pAlpha
    ]
  return $ foldl Application f st

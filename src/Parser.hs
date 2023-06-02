module Parser (
  pAlpha
, pLambda
, pBraces
, pExpr
) where

import Text.Megaparsec ( Parsec, some, choice, satisfy )

import Types (VarName, Expression (Variable, Application, Abstraction))
import Data.Void (Void)
import Data.Char (isAsciiLower)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void String

pAlpha :: Parser VarName
pAlpha = satisfy isAsciiLower

pLambda :: Parser (Expression -> Expression)
pLambda = do
  _ <- choice [ char '|', char '\\' ]
  vs <- some pAlpha
  _ <- char '.'
  return $ foldl (.) id (map Abstraction vs)

pBraces :: Parser Expression
pBraces = do
  _ <- char '('
  expr <- pExpr
  _ <- char ')'
  return expr

pExpr :: Parser Expression
pExpr = do
  f:st <- some $ choice [
    pBraces
    , pLambda <*> pExpr
    , Variable <$> pAlpha
    ]
  return $ foldl Application f st


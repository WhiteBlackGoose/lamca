module Parser (
  parse
) where

import Text.Megaparsec ( Parsec, some, choice, satisfy, MonadParsec (eof) )
import qualified Text.Megaparsec (parse)

import Types (VarName, Expression (Variable, Application, Abstraction))
import Data.Void (Void)
import Data.Char (isAsciiLower)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void String

pAlpha :: Parser VarName
pAlpha = satisfy isAsciiLower

pLambda :: Parser (Expression -> Expression)
pLambda = foldl (.) id . map Abstraction
    <$> (choice [ char '|', char '\\' ] *> some pAlpha <* char '.')

pBraces :: Parser Expression
pBraces = char '(' *> pExpr <* char ')'

pExpr :: Parser Expression
pExpr = do
  f:st <- some $ choice [
    pBraces
    , pLambda <*> pExpr
    , Variable <$> pAlpha
    ]
  return $ foldl Application f st

parse = Text.Megaparsec.parse (pExpr <* eof :: Parser Expression) ""

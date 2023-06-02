module Main where

import Operations (vars, sub, alpha, beta, eta)
import Types (VarName, Expression (Variable, Application, Abstraction))
import Parser (pLambda)

import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (char)
type Parser = Parsec Void String
mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

main :: IO ()
main = putStrLn "Hello, Haskell!"

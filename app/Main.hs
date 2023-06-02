module Main where

import Operations (vars, sub, alpha, beta, eta)
import Types (VarName, Expression (Variable, Application, Abstraction))

main :: IO ()
main = putStrLn "Hello, Haskell!"

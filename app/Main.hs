module Main where

import Operations (vars, sub, alpha, beta, eta)
import Parser (parse)
import Control.Monad (forever)


respond :: String -> String
respond (r:' ':expr) = case r of
  'a' -> mp2 (parse <$> words expr) alpha
  'b' -> mp beta
  'e' -> mp eta
  'v' -> mp (show . vars)
  's' -> case expr of
    x:' ':rest -> mp2 (parse <$> words rest) (sub x)
    _ -> "Substitute requires a certain syntax"
  _ -> "Unrecognized command: " ++ [r]
  where
    mp app = case parse expr of
      Left er -> "Error: \n" ++ show er
      Right ex -> show $ app ex
    mp2 w app =
      case w of
        [ Left er1, Left er2 ] -> "Two errors: \n" ++ show er1 ++ "\n" ++ show er2
        [ Right _, Left er ] -> "One error: \n" ++ show er
        [ Left er, Right _ ] -> "One error: \n" ++ show er
        [ Right ex1, Right ex2 ] -> show $ app ex1 ex2
        _ -> "Unexpected number of expressions"
respond _ = "Format as: r:' ':expr"

main :: IO ()
main = forever (getLine >>= putStrLn . respond)

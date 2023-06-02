module Main where
import Test.HUnit (Assertion, runTestTT, Counts (Counts))
import ParserTest (parserTestSuccess, parserTestFailures)
import OperationsTest (varsTests, subTests, alphaTests, betaTests, etaTests)
import Control.Monad (forM, foldM, (<=<))
import Data.Foldable (traverse_)

main :: IO ()
main = traverse_ (print <=< runTestTT) [
      parserTestSuccess
    , parserTestFailures
    , varsTests
    , subTests
    , betaTests
    , etaTests
    ]


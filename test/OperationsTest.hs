module OperationsTest (
  betaTests
) where
import Test.HUnit (Assertion, Test (TestList, TestCase), assertEqual, assertFailure)
import Text.Megaparsec (parse)
import Parser (pExpr)
import Operations (beta)

data TPair = String := String

betaTests = TestList $ map (TestCase . p) [
    "x"              := "x"
  , "|x.x"           := "|x.x"
  , "xy"             := "xy"
  , "(|x.x)y"        := "y"
  , "xx"             := "xx"
  , "(|x.x)(|y.y)z"  := "z"
  , "(|x.x)(|y.y)y"  := "y"
  , "(|x.xxy)y"      := "yyy"
  , "(|x.xx)yy"      := "yyy"
  , "(|x.xxx)abcd"   := "aaabcd"
  , "(|x.xxx)(|x.x)" := "|x.x"
  , "(|x.xxx)(yz)"   := "yz(yz)(yz)"
  ]
  where
    p (s1 := s2) =
      case (parse pExpr "" s1, parse pExpr "" s2) of
        (Right e1, Right e2) ->
          assertEqual "" (beta e1) e2
        _ -> assertFailure "Parse error"



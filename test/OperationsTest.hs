module OperationsTest (
  varsTests
, subTests
, betaTests
) where
import Test.HUnit (Assertion, Test (TestList, TestCase), assertEqual, assertFailure)
import Parser (parse)
import Operations (beta, vars, sub)

data TPair = String := String

data TTriple = TPair :=> String

varsTests = TestList $ map (TestCase . p) [
  -- expression      variables
    "abc"         := "abc"
  , "|x.x"        := ""
  , "|x.yx"       := "y"
  , "|x.yxy"      := "y"
  , "|yxz.yc"     := "c"
  , "|yxz.abcyex" := "abce"
  , "x(|x.x)"     := "x"
  ]
  where
    p (s := vs) = case parse s of
      Right e -> assertEqual ("Vars of "++s) vs (vars e)
      _ -> assertFailure ("Parse error: " ++ s)

subTests = TestList $ map (TestCase . p) [
    "y" := "x" :=> "y"
  , "x" := "x" :=> "x"
  , "yx" := "|x.x" :=> "|x.x"
  , "yx" := "|y.yx" :=> "|a.a(yx)"
  ]
  where
    p (((exprToSub := orig) :=> final)) =
      case (parse exprToSub, parse orig, parse final) of
        (Right e0, Right e1, Right e2) ->
          assertEqual "sub" (sub 'x' e0 e1) e2
        _ -> assertFailure "Parse error"

betaTests = TestList $ map (TestCase . p) [
  -- input              beta reduced
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
      case (parse s1, parse s2) of
        (Right e1, Right e2) ->
          assertEqual ("β("++s1++")!="++s2) (beta e1) e2
        _ -> assertFailure ("Parse error: " ++ s1 ++ " " ++ s2)



module OperationsTest (
  varsTests
, subTests
, alphaTests
, betaTests
, etaTests
) where
import Test.HUnit (Assertion, Test (TestList, TestCase), assertEqual, assertFailure)
import Parser (parse)
import Operations (beta, vars, sub, alpha, eta)

data TPair = String := String

data TTriple a = TPair :=> a

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
    "y"  := "x"     :=> "y"
  , "x"  := "x"     :=> "x"
  , "yx" := "|x.x"  :=> "|x.x"
  , "yx" := "|y.yx" :=> "|a.a(yx)"
  ]
  where
    p (((exprToSub := orig) :=> final)) =
      case (parse exprToSub, parse orig, parse final) of
        (Right e0, Right e1, Right e2) ->
          assertEqual "sub" (sub 'x' e0 e1) e2
        _ -> assertFailure "Parse error"

alphaTests = TestList $ map (TestCase . p) [
    "x" := "x" :=> True
  , "y" := "x" :=> False
  , "|x.x" := "|y.y" :=> True
  , "|x.u" := "|y.y" :=> False
  , "|xy.xy" := "|yx.yx" :=> True
  , "|xy.xyu" := "|yx.yxu" :=> True
  , "|xy.xyu" := "|yx.xyu" :=> False
  , "x(|x.x)" := "y(|y.y)" :=> False
  , "y(|x.x)" := "y(|y.y)" :=> True
  ]
  where
    p (s1 := s2 :=> out) =
      case (parse s1, parse s2) of
        (Right e1, Right e2) ->
          assertEqual "alpha" (alpha e1 e2) out
        _ -> assertFailure ("Parse error: " ++ s1 ++ " " ++ s2)

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

etaTests = TestList $ map (TestCase . p) [
    "x"              := "x"
  , "yx"             := "yx"
  , "|x.yx"          := "y"
  , "|x.xy"          := "|x.xy"
  , "|x.yux"         := "yu"
  , "|x.(|u.yu)x"      := "y"
  , "|x.(|u.(|abc.ycba)u)x"  := "(|abc.ycba)"
  , "|xuabc.ycbauxc" := "|xuab.ycbaux"
  ]
  where
    p (s1 := s2) =
      case (parse s1, parse s2) of
        (Right e1, Right e2) ->
          assertEqual ("η("++s1++")!="++s2) (eta e1) e2
        _ -> assertFailure ("Parse error: " ++ s1 ++ " " ++ s2)

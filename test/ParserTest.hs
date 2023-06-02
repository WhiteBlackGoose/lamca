module ParserTest (
  parserTestSuccess
, parserTestFailures
) where
import Test.HUnit (Assertion, assertEqual, Test (TestCase), assertBool)
import Parser (pExpr)
import Types (Expression(..))
import Text.Megaparsec (parse)
import Test.HUnit.Base (Test(TestList))

parserTestSuccess = TestList $ map TestCase [
    pe "x"           $ v 'x'
  , pe "(x)"         $ v 'x'
  , pe "xy"          $ a (v 'x') (v 'y')
  , pe "xyz"         $ a (a (v 'x') (v 'y')) (v 'z')
  , pe "x(yz)"       $ a (v 'x') (a (v 'y') (v 'z'))
  , pe "|x.x"        $ l 'x' (v 'x')
  , pe "(|x.x)"      $ l 'x' (v 'x')
  , pe "(|x.x)y"     $ a (l 'x' (v 'x')) (v 'y')
  , pe "(|x.x)yx"    $ a (a (l 'x' (v 'x')) (v 'y')) (v 'x')
  , pe "(|x.x)yxz"   $ a (a (a (l 'x' (v 'x')) (v 'y')) (v 'x')) (v 'z')
  , pe "(|x.x)y(xz)" $ a (a (l 'x' (v 'x')) (v 'y')) (a (v 'x') (v 'z'))
  , pe "|xy.xy"      $ l 'x' (l 'y' (a (v 'x') (v 'y')))
  ]
  where
    p = parse pExpr ""
    l = Abstraction
    a = Application
    v = Variable
    pe :: String -> Expression -> Assertion
    pe s v = assertEqual s (p s) (Right v)

parserTestFailures = TestList $ map TestCase [
    u "x_"
  , u " "
  , u "|x."
  , u "|.u"
  , u "(x"
  , u "x)"
  , u "((x)"
  , u "())(x()"
  , u "()"
  ]
  where
    u e = assertBool e $ case parse pExpr "" e of Left _ -> True; Right _ -> False

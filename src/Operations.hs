module Operations (
  vars
, sub
, alpha
, beta
, eta
) where

import Types (VarName, Expression (Variable, Application, Abstraction))

vars :: Expression -> [VarName]
vars (Variable v) = [v]
vars (Abstraction x ex) = filter (/=x) . vars $ ex
vars (Application a b) = vars a ++ vars b

sub :: VarName -> Expression -> Expression -> Expression
sub x expr (Variable y)
  | x == y = expr
  | otherwise = Variable y
sub x expr (Application f y) = Application (sub x expr f) (sub x expr y)
sub x expr (Abstraction v e)
  | v == x = Abstraction v e
  | v `notElem` used = Abstraction v $ sub x expr e
  | otherwise =
    let
      newv = head . filter (`notElem` used) $ ['a'..'z']
      newe = sub v (Variable newv) e
    in
      Abstraction newv (sub x expr newe)
    where used = vars expr ++ (filter (/=v) . vars $ e)



alpha :: Expression -> Expression -> Bool
alpha (Variable x) (Variable y) = x == y
alpha (Application a1 b1) (Application a2 b2) =
  alpha a1 a2 && alpha b1 b2
alpha (Abstraction x1 expr1) (Abstraction x2 expr2)
  | x1 == x2 = alpha expr1 expr2
  | otherwise = alpha expr1 (sub x2 (Variable x1) expr2)
alpha _ _ = False


beta :: Expression -> Expression
beta (Variable x) = Variable x
beta (Abstraction x expr) = Abstraction x (beta expr)
beta (Application expr value) = case beta expr of
  Abstraction x body ->  beta (sub x value body)
  other -> Application other (beta value)

eta :: Expression -> Expression
eta (Abstraction x (Application expr (Variable x'))) | x == x' = expr 
eta (Abstraction x ex) = Abstraction x (eta ex)
eta (Application a b) = Application (eta a) (eta b)
eta other = other

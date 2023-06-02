module Types (
  VarName
, Expression (Variable, Abstraction, Application)
) where

type VarName = Char

data Expression =
    Variable VarName
    | Abstraction VarName Expression
    | Application Expression Expression
    deriving (Eq)

instance Show Expression where
  show :: Expression -> String
  show (Variable v) = [v]
  show (Abstraction x ex) =  '\\' : x : '.' :  show ex
  show (Application (Abstraction x ex) expr) = "(" ++ show (Abstraction x ex) ++ ")" ++ show expr
  show (Application (Variable a) (Variable b)) = [a, b]
  show (Application (Variable a) expr) = a : '(' : show expr ++ ")"
  show (Application e1 expr) = show e1 ++ show expr


module Types (
  VarName
, Expression (Variable, Abstraction, Application)
) where

type VarName = Char

data Expression =
    Variable VarName
    | Abstraction VarName Expression
    | Application Expression Expression


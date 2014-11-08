
module LangAst where

data Token = 
    Symbol String
    | Arrow
    | LParen
    | RParen
    | Assign
    | Lambda
    | NewLine
    | Space Int

    deriving (Show, Eq)

data Scope = Scope String [Token]

    deriving (Show, Eq)

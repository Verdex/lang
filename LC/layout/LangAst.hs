
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


module LangAst where

data Token = Symbol String
           | Assign
           | Lambda
           | RArrow
           | Colon
           | Semicolon
           | RParen
           | LParen
           | Type 
    deriving (Show, Eq)


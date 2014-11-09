
module LangAst where

data Token = Symbol String
           | Match
           | With
           | Let 
           | In
           | End
           | Assign
           | Lambda
           | RArrow
           | Underscore
           | Colon
           | Semicolon
           | RParen
           | LParen


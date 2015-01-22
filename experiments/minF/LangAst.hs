
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

data Program = Program [TopLevel]
             | ParseError
    deriving (Show, Eq)

data TopLevel = Define ValueDef 
              | DefineSig ValueSig
              | TypeDefine TypeDef 
    deriving (Show, Eq)

data Expr = EVar String
          | EAbs [String] Expr
          | EApp Expr Expr
          | Undefined
    deriving (Show, Eq)

data ValueSig = ValueSig String TypeSig
    deriving (Show, Eq)

data ValueDef = ValueDef String Expr
    deriving (Show, Eq)

data TypeDef = TypeDef String [String]
    deriving (Show, Eq)

data TypeSig = TSingle String
             | TypeApp String [TypeSig]
             | TArrow TypeSig TypeSig 
    deriving (Show, Eq)

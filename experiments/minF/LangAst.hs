
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
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data ValueSig = ValueSig String TypeSig
    deriving (Show, Eq)

data ValueDef = ValueDef String Expr
    deriving (Show, Eq)

data TypeDef = TypeDef String [String]
    deriving (Show, Eq)

data TypeSig = TSingle String
             | TApp TypeSig TypeSig
             | TArrow TypeSig TypeSig 
    deriving (Show, Eq)

data Type = Variable Int 
          | Simple String
          | Indexed String [Type] 
          | Arrow Type Type
          | Forall Int Type
    deriving Show

-- I'm not sure if my Variable and Forall equality is correct
instance Eq Type where
    Variable _ == Variable _ = True 
    Simple n == Simple n' = n == n'
    Indexed n ts == Indexed n' ts' = n == n' && ts == ts'
    Arrow ta tb == Arrow ta' tb' = ta == ta' && tb == tb'
    Forall _ t == Forall _ t' = t == t'


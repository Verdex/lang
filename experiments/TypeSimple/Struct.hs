
module Struct where


type Env = [(String, Type)]

data Expr = EVar String
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data Type = TSimple String
          | TArrow Type Type
          | TVar Integer
    deriving (Show, Eq)



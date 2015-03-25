
module Simple where

import Control.Applicative

data Expr = EVar String
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data Type = Simple String
          | Arrow Type Type
    deriving Show


typeOf :: Expr -> Maybe Type
typeOf = undefined

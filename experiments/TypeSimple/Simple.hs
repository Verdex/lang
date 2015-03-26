
module Simple where

import Control.Applicative
import State

data Expr = EVar String
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data Type = TSimple String
          | TArrow Type Type
          | TVar Integer
    deriving Show

type Env = [(String, Type)]

typeOf :: Env -> Expr -> Maybe Type
typeOf env expr = evalState (typeOf' env expr) 0
    where 
        typeOf' env (EVar varName) = pure $ lookup varName env
        typeOf' env (EAbs paramName expr) =
            do
                i <- newInteger
                exprType <- typeOf' ( (paramName, TVar i) : env ) expr
                return $ TArrow (TVar i) <$> exprType 

        typeOf' env (EApp e1 e2) = undefined


newInteger :: State Integer Integer
newInteger = State $ \ i -> (i + 1, i)


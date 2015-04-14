
module Simple where

import Control.Applicative
import State
import Struct
import TypeUnifier

typeOf :: Env -> Expr -> Maybe Type
typeOf env expr = evalState (typeOf' env expr) 0
    where 
        typeOf' env (EVar varName) = pure $ lookup varName env
        typeOf' env (EAbs paramName expr) =
            do
                i <- newInteger
                exprType <- typeOf' ( (paramName, TVar i) : env ) expr
                return $ TArrow (TVar i) <$> exprType 

        typeOf' env (EApp e1 e2) = 
            do
                t1 <- typeOf' env e1
                t2 <- typeOf' env e2
                blarg $ unifyApply <$> t1 <*> t2

-- blarg is very interesting by itself ...
blarg :: Maybe (State Integer (Maybe Type)) -> State Integer (Maybe Type)
blarg (Just m) = m
blarg Nothing = pure Nothing





module TypeCheck where

import Control.Applicative
import LangAst
import Unique


type Env = [(String, Type)] 

typeOf :: Env -> Expr -> Maybe Type
typeOf env e = evalUnique (typeOf' env e) 0
    where 
        typeOf' env (EVar name) = pure $ lookup name env

        typeOf' env (EAbs n e) = 
            do
                v <- newInteger
                inner <- typeOf' ((n, Variable v) : env) e 
                return ( (Forall v . Arrow (Variable v)) <$> inner )

        typeOf' env (EApp e1 e2) = 
            do
                t1 <- typeOf' env e1 
                t2 <- typeOf' env e2
                return $ mu $ morph <$> t1 <*> t2 

            where morph (Arrow input output) t
                      | input == t = Just output 
                      | otherwise = Nothing
                  morph (Forall i inner) t = morph (sub t i inner) t
                  morph _ _ = Nothing 

                  mu Nothing = Nothing
                  mu (Just Nothing) = Nothing
                  mu (Just (Just t)) = Just t

                  sub replace i (Variable n)
                      | i == n = replace
                      | otherwise = Variable n
                  sub replace i (Indexed n ts) = Indexed n $ map (sub replace i) ts
                  sub replace i (Arrow t1 t2) = Arrow (sub replace i t1) (sub replace i t2)
                  sub replace i (Forall n inner) = Forall n (sub replace i inner)
                  sub _ _ (Simple n) = Simple n


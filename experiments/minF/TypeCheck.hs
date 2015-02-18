

module TypeCheck where

import Control.Applicative
import LangAst


type Env = [(String, Type)] 

typeOf :: Env -> Expr -> Maybe Type
typeOf env expr = typeOf' 0 env expr
    where 
        typeOf' _ env (EVar name) =  lookup name env

        -- putting (n, Variable i) on the front of the list will allow
        -- expected lexical scoping (closer is used) to function automatically
        typeOf' tVarI env (EAbs n e) = Forall tVarI <$> (typeOf' (tVarI+1) ((n, Variable tVarI) : env) e)

        typeOf' tVarI env (EApp e1 e2) = 
            let t1 = typeOf' tVarI env e1 in
            let t2 = typeOf' tVarI env e2 in
                 mu $ morph <$> t1 <*> t2 

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


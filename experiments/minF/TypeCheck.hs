

module TypeCheck where

import Control.Applicative
import LangAst
-- x : int
-- blah : int
-- blah = x 

-- blah : int
-- blah = id x
-- id : a -> a
-- app (a->a) int => int

-- app (int -> int) int => int

-- app (int -> ?) int => okay
-- app (? -> int) ? => int



-- type info for globals can come from their type sig
-- type info for params can come from the function type sig
-- i guess mapping function type sig to params might be kind of
-- hard especially when a function ends up being defined in terms
-- of an apply that returns a function



-- 
-- type var "a" => lookup "a"
-- type app e1 e2 => match type e1 with
--                   |  not arrow type -> error
--                   |  arrow a _      -> if a != type e2 -> error
--                   |  arrow _ b      -> b
-- type abs params expr => forall params . params -> type expr 
--                Im going to need some way to unify forall vars 
--                  (both inside a lambda and when you apply a lambda to another) 
--                    ( \ a -> a ) ( \ a -> a ) => app (forall a . a -> a) (forall a . a -> a)
--                      although it looks like a sufficiently advanced lookup might be able to handle that

type Lookup = String -> Maybe Type

-- if you have an app that generates a function then our type sig will be describing
-- params that dont exist in the current lexical scope.  we stil need to type check
-- but we dont need to worry about looking up anything that those sigs describe (not in lex scope).



-- blah : x -> y -> z
-- blah = someFunc (\ w -> ... )
-- for that case none of the blah type anotation can possibly apply 
-- to the lambda

-- blah : x -> y -> z
-- blah = \ w -> \ ww -> ...
-- here we know that the type annotation applies

-- t1 = forall a . forall b . a -> b -> a
-- t2 = X
-- t3 = Y
-- (\ x -> \ y -> x) X Y


typeOf :: Lookup -> Expr -> Maybe Type
typeOf env (EVar name) = env name
typeOf env (EApp e1 e2) = 
    let t1 = typeOf env e1 in
    let t2 = typeOf env e2 in
         mu $ morph <$> t1 <*> t2 

    where morph (Arrow input output) t
              | input == t = Just output 
              | otherwise = Nothing
          morph (Forall i inner) t = morph (sub i inner) t
          morph _ _ = Nothing 

          mu Nothing = Nothing
          mu (Just Nothing) = Nothing
          mu (Just (Just t)) = Just t

          sub = undefined

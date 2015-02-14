

module TypeCheck where

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

-- need some sort of type equivalence that handles forall

-- the existence of lookup makes me think this is going to involve state monad

data Type = Variable String 
          | Single String
          | App String [Type] 
          | Arrow Type Type
          | Forall [String] Type

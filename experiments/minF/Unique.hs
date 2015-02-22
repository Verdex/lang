
module Unique where

-- Note:  This is basically the State monad.

import Control.Applicative

data Unique s a = Unique ( s -> (s, a) ) 

proj (Unique f) = f

evalUnique :: Unique s a -> s -> a
evalUnique u s = let (_, r) = proj u s in r

instance Functor (Unique s) where
    fmap f a = Unique $ \ s -> let (s', r) = proj a s in (s', f r)

instance Applicative (Unique s) where
    pure a = Unique $ \ s -> (s, a)
    u1 <*> u2 = 
        do
            f <- u1
            a <- u2
            return $ f a

instance Monad (Unique s) where
    return = pure
    u >>= gen = Unique $ \ s -> 
        let (s', r) = proj u s in proj (gen r) s'


newInteger :: Unique Integer Integer
newInteger = Unique $ \ i -> (i + 1, i)


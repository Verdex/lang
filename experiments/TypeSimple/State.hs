
module State where

-- So this is a naive implementation of a State monad.
-- The bad news is that is sounds like the "proper" haskell
-- state monad is significantly better than this guy (fast
-- mutable data that can't escape the monad).  The good news
-- is that I understand my naive implementation right now
-- (as opposed to the cool one which I do not).  Clearly
-- my reading list needs to be expanded.

import Control.Applicative

data State s a = State ( s -> (s, a) ) 

proj (State f) = f

evalState :: State s a -> s -> a
evalState u s = let (_, r) = proj u s in r

getState :: State s s
getState = State $ \ s -> (s, s)

instance Functor (State s) where
    fmap f a = State $ \ s -> let (s', r) = proj a s in (s', f r)

instance Applicative (State s) where
    pure a = State $ \ s -> (s, a)
    u1 <*> u2 = 
        do
            f <- u1
            a <- u2
            return $ f a

instance Monad (State s) where
    return = pure
    u >>= gen = State $ \ s -> 
        let (s', r) = proj u s in proj (gen r) s'



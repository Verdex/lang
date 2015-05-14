
module State where


import Control.Applicative

data State s a = State ( s -> (s, a) ) 

proj (State f) = f

evalState :: State s a -> s -> a
evalState u s = let (_, r) = proj u s in r

finalState :: State s a -> s -> s
finalState u s = let (final, _) = proj u s in final

evalFinalState :: State s a -> s -> (s, a)
evalFinalState u s = let (final, r) = proj u s in (final, r)

getState :: State s s
getState = State $ \ s -> (s, s)

setState :: s -> State s ()
setState newState = State $ \ s -> (newState, ())

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



module Unify where

import Control.Applicative

data MSResult s a = Success s a
                | Failure 
data MState s a = MState ( s -> MSResult s a )
proj (MState f) = f
    
instance Functor (MState s) where
    fmap f a = MState $ \ s -> handleResult $ proj a s 
        where handleResult (Success s a) = Success s (f a)
              handleResult Failure = Failure
instance Applicative (MState s) where
    pure a = MState $ \ s -> Success s a
    ms1 <*> ms2 = 
        do
            f <- ms1
            a <- ms2
            return $ f a
instance Monad (MState s) where
    return = pure
    ms >>= gen = mu $ fmap gen ms
        where mu ms = MState $ \ s -> inner $ proj ms s 
              inner Failure = Failure
              inner (Success s a) = proj a s 

failure :: MState s a
failure = MState $ \ s -> Failure

success :: a -> MState s a
success = pure

finalMState :: MState s a -> s -> Maybe s
finalMState ms init = hr $ proj ms init
    where hr (Success s a) = Just s
          hr Failure = Nothing

-- need get and set

data Term = Constant String
          | Variable Integer
          | Function String [Term]
    deriving Show

type Env = [(Integer, Term)]

unify :: Env -> Term -> Term -> Maybe Env
unify env (Constant x) (Constant y) = if x == y then Just env else Nothing
unify env (Function n1 ts1) (Function n2 ts2) = if n1 == n2 && (length ts1) == (length ts2) 
                                                then Nothing 
                                                else Nothing
    --where unifyChildren env (t1:ts1) (t2:ts2) = unify

unify' :: Env -> Term -> Term -> Maybe Env
unify' env t1 t2 = finalMState (unify'' t1 t2) env

unify'' :: Term -> Term -> MState Env () 
unify'' (Constant x) (Constant y) = 
    if x == y then success ()
              else failure 

unify'' (Function n1 ts1) (Function n2 ts2) = 
    if n1 == n2 && (length ts1) == (length ts2)
    then 
        do 
            unifyAll ts1 ts2
    else failure 

    where unifyAll [] [] = pure ()
          unifyAll (t1:ts1) (t2:ts2) =
            do
                unify'' t1 t2
                unifyAll ts1 ts2 


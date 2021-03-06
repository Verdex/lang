
module Unify where

import Control.Applicative
import Data.List
import Data.Traversable

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

success :: MState s ()
success = pure ()

finalMState :: MState s a -> s -> Maybe s
finalMState ms init = hr $ proj ms init
    where hr (Success s a) = Just s
          hr Failure = Nothing

getState :: MState s s
getState = MState $ \ s -> Success s s

setState :: s -> MState s ()
setState s = MState $ \ s' -> Success s () 


data Term = Constant String
          | Variable Integer
          | Function String [Term]
    deriving (Show, Eq)

type Env = [(Integer, Term)]

addToEnv :: Integer -> Term -> MState Env ()
addToEnv i t = 
    do
        env <- getState
        setState $ (i, t) : env

checkEnv :: Integer -> MState Env (Maybe Term)
checkEnv i = 
    do
        env <- getState
        return $ lookup i env

unify :: Env -> Term -> Term -> Maybe Env
unify env t1 t2 = finalMState (unify' t1 t2) env

unify' :: Term -> Term -> MState Env () 
unify' (Constant x) (Constant y) = 
    if x == y then success
              else failure 

unify' (Function n1 ts1) (Function n2 ts2) = 
    if n1 == n2 && (length ts1) == (length ts2)
    then 
        do 
            traverse (\ (t1, t2) -> unify' t1 t2) (zip ts1 ts2)
            return ()
    else failure 

unify' a@(Variable ai) t =
    do
        rt <- replaceVar t
        occurs' a rt
        menv <- checkEnv ai
        case menv of
            Nothing -> do { addToEnv ai t ; backfill } -- TODO addToEnv ai t should probably be addToEnv ai rt
            Just t' -> unify' t t'

unify' t a@(Variable _) = unify' a t

unify' _ _ = failure 

occurs a@(Variable _) b@(Variable _) = a == b
occurs (Variable _) (Constant _) = False
occurs a@(Variable _) (Function _ ts) = any (occurs a) ts

occurs' :: Term -> Term -> MState Env ()
occurs' a@(Variable _) b@(Variable _)
    | a == b = failure
    | otherwise = success
occurs' (Variable _) (Constant _) = success 
occurs' a@(Variable _) (Function _ ts) = occursAny ts

    where occursAny [] = success 
          occursAny (t:ts) = 
            do 
                occurs' a t
                occursAny ts


-- TODO here's the thing.  because I'm backfilling after every variable alias, I should be able to
-- just backfill the one new assignment that actually changed (something for stable)
backfill :: MState Env () 
backfill = 
    do
        env <- getState
        let newEnv = map (\ (i, t) -> (i, varReplace env t) ) env in setState newEnv 

varReplace env t@(Variable ti) = 
    case lookup ti env of
        Nothing -> t
        Just t' -> varReplace env t'
varReplace env t@(Constant _) = t
varReplace env (Function n ts) = Function n $ map (varReplace env) ts

replaceVar :: Term -> MState Env Term
replaceVar t@(Constant _) = pure t
replaceVar (Function n ts) = Function n <$> traverse replaceVar ts


replaceVar t@(Variable ti) = 
    do
        mt <- checkEnv ti
        case mt of
            Nothing -> pure t
            Just t' -> replaceVar t'

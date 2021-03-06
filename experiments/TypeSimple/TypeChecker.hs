
module TypeChecker where

import Control.Applicative
import MState

type TypeEnv = [(String, Type)]

data Expr = EVar String
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data Type = TSimple String
          | TArrow Type Type
          | TVar Integer
    deriving (Show, Eq)


type VarEnv = [(Integer, Term)]
-- going to want a single environment for the monad
-- however the typeOf probably should only expose the Type Env but not the 
-- variable unification environment


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



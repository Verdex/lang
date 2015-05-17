
module TypeCheck where

import Control.Applicative
import State

type VarName = String
type Ctx = [(VarName, Type)]

type InferNum = Integer
type Facts = [(InferNum, Maybe Type)]

data Expr = EVar VarName 
          | EAbs VarName Type Expr
          | EApp Expr Expr
          deriving (Show, Eq)

data Type = TVar String
          | TVar' Integer
          | TSimple String
          | TArrow Type Type
          deriving (Show, Eq)


-- TODO not sure what to call this type right now
type N = (Integer, [(String, Integer)])

newInt :: State N Integer
newInt = 
    do
        (s, ctx) <- getState
        setState (s + 1, ctx)
        return s

setLink :: String -> Integer -> State N ()
setLink n i = 
    do
        (s, ctx) <- getState
        setState $ (s, (n, i) : ctx)

lookupLink :: String -> State N (Maybe Integer)
lookupLink n = 
    do
        (_, ctx) <- getState
        return $ lookup n ctx

freeVarShift :: Integer -> Type -> (Integer, Type)
freeVarShift i t = evalState (freeVarShift' t) (i, [])
freeVarShift' :: Type -> State N (Integer, Type)
freeVarShift' (TVar s) = 
    do
        maybe_int <- lookupLink s
        case maybe_int of
            Nothing -> do
                            int <- newInt
                            setLink s int
                            return (int, (TVar' int))
            Just int -> return (int, (TVar' int))
freeVarShift' t =
    do
        int <- newInt
        return (int, t)


failure :: State a (Maybe b)
failure = pure Nothing



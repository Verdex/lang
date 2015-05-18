
module TypeCheck where

import Control.Applicative
import State

type VarName = String
type Ctx = [(VarName, Type)]


data Expr = EVar VarName 
          | EAbs VarName Type Expr
          | EApp Expr Expr
          deriving (Show, Eq)

data Type = TVar String
          | TVar' Integer
          | TSimple String
          | TArrow Type Type
          deriving (Show, Eq)

type UB = (Integer, [(String, Integer)])

newInt :: State UB Integer
newInt = 
    do
        (s, ctx) <- getState
        setState (s + 1, ctx)
        return s

setLink :: String -> Integer -> State UB ()
setLink n i = 
    do
        (s, ctx) <- getState
        setState $ (s, (n, i) : ctx)

lookupLink :: String -> State UB (Maybe Integer)
lookupLink n = 
    do
        (_, ctx) <- getState
        return $ lookup n ctx

freeVarShift :: Type -> State Integer Type
freeVarShift t = 
    do
        init <- getState 
        ((final, _), t) <- pure $ evalFinalState (freeVarShift' t) (init, [])
        setState final
        return t

freeVarShift' :: Type -> State UB Type
freeVarShift' (TVar s) = 
    do
        maybe_int <- lookupLink s
        case maybe_int of
            Nothing -> do
                            int <- newInt
                            setLink s int
                            return (TVar' int)
            Just int -> return (TVar' int)
freeVarShift' t = pure t

failure :: State a (Maybe b)
failure = pure Nothing


typeof :: Ctx -> Ctx -> Expr -> (Maybe Type)
typeof freeVars boundVars e = evalState (typeof' freeVars boundVars e) 0

typeof' :: Ctx -> Ctx -> Expr -> State Integer (Maybe Type)
typeof' freeVars boundVars (EVar s) =  (<|>) <$> (pure $ lookup s boundVars) <*> (ikky $ freeVarShift <$> lookup s freeVars)
    where ikky Nothing = pure Nothing
          ikky (Just s) = fmap Just s

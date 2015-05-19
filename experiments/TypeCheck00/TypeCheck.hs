
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
freeVarShift' t@(TSimple _) = pure t
freeVarShift' t@(TVar' _) = pure t
freeVarShift' (TArrow t1 t2) = 
    do
        t1' <- freeVarShift' t1
        t2' <- freeVarShift' t2
        return $ TArrow t1' t2'

failure :: State a (Maybe b)
failure = pure Nothing


typeof :: Ctx -> Expr -> (Maybe Type)
typeof free e = evalState (typeof' free [] e) 0 

typeof' :: Ctx -> Ctx -> Expr -> State Integer (Maybe Type)
typeof' free bound (EVar s) =  (<|>) <$> (pure $ lookup s bound) <*> (ikky $ freeVarShift <$> lookup s free)
    where ikky Nothing = pure Nothing
          ikky (Just s) = fmap Just s

typeof' free bound (EAbs n t e) = typeof' free ( (n, t) : bound ) e

typeof' free bound (EApp e1 e2) = 
    do
        maybe_t1 <- typeof' free bound e1
        maybe_t2 <- typeof' free bound e2
        case maybe_t1 of
            Nothing -> failure
            Just (TSimple _) -> failure
            Just (TVar _) -> failure
            Just (TArrow t11 t12) -> failure



unify :: Type -> Type -> Maybe Type
unify o@(TSimple a) (TSimple b) = if a == b then Just o else Nothing

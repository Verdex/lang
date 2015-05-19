
module TypeCheck where

import Control.Applicative
import State
import UniqueBinding

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

failure :: State a (Maybe b)
failure = pure Nothing

freeVarShift :: Type -> State (UB Integer Type) Type
freeVarShift t = 
    do
        (init, ctx) <- getState 
        ((final, _), t) <- pure $ evalFinalState (freeVarShift' t) (init, [])
        setState (final, ctx)
        return t

freeVarShift' :: Type -> State (UB String Integer) Type
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

typeof :: Ctx -> Expr -> (Maybe Type)
typeof free e = evalState (typeof' free [] e) nullBinding 

typeof' :: Ctx -> Ctx -> Expr -> State (UB Integer Type) (Maybe Type)
typeof' free bound (EVar s) =  (<|>) <$> (pure $ lookup s bound) <*> (ikky $ freeVarShift <$> lookup s free)
    where ikky Nothing = pure Nothing
          ikky (Just s) = fmap Just s

typeof' free bound (EAbs n t e) = typeof' free ( (n, t) : bound ) e

typeof' free bound (EApp e1 e2) = 
    do
        maybe_t1 <- typeof' free bound e1
        maybe_t2 <- typeof' free bound e2
        typeofApp maybe_t1 maybe_t2

typeofApp :: Maybe Type -> Maybe Type -> State (UB Integer Type) (Maybe Type)
typeofApp Nothing _ = failure
typeofApp _ Nothing = failure
typeofApp (Just (TSimple _)) _ = failure
typeofApp (Just (TVar _)) _ = failure
typeofApp (Just (TArrow t11 t12)) (Just t2) = 
    do
        r <- unify t11 t2
        case r of
            Nothing -> failure
            (Just _) -> return $ Just t12 
typeofApp (Just t1@(TVar' _)) (Just t2) = 
    do
        newTVar' <- fmap TVar' newInt
        r <- unify t1 (TArrow t2 newTVar')
        case r of
            Nothing -> failure
            (Just _) -> return $ Just newTVar' 

unify :: Type -> Type -> State (UB Integer Type) (Maybe Type)
unify t@(TSimple a) (TSimple b) = if a == b then pure $ Just t else failure
unify t@(TVar a) (TVar b) = if a == b then pure $ Just t else failure
unify t@(TArrow t11 t12) (TArrow t21 t22) = 
    do
        r1 <- unify t11 t21
        case r1 of
            Nothing -> failure
            (Just _) -> do
                            r2 <- unify t12 t22
                            case r2 of
                                Nothing -> failure
                                (Just _) -> return $ Just t
unify t1@(TVar' i1) t2 =  -- TODO occurs check?
    do
        maybe_l1 <- lookupLink i1
        case maybe_l1 of
            Nothing -> unify t1 t2
            Just t -> unify t2 t

unify _ _ = failure
       


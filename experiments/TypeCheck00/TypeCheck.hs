
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

typeof :: Ctx -> Expr -> Maybe Type
typeof ctx e = resolve $ evalFinalState (typeof' ctx e) nullEngine

resolve :: (InferEngine, Maybe Type) -> Maybe Type
resolve = undefined

-- unify for infer will add to the Facts which need to be resolved
-- unify for TVar will need to overwrite things in ctx or something (add new with same name but higher in list?)

-- unify for TVar seems like its going to effect all other TVars with the same name, but only locally
-- to make things more complicated I think that it should probably effect Infers that point to matching
-- TVars (but again only "locally")

-- it might be best to get ETypedAbs working first

typeof' :: Ctx -> Expr -> State InferEngine (Maybe Type)
typeof' ctx (EVar n) = pure $ lookup n ctx
typeof' ctx (EAbs n t e) = typeof' ( (n, t) : ctx ) e
{-typeof' ctx (EInferAbs n e) = 
    do
        infer <- fmap TInfer newInfer
        typeof' ( (n, infer) : ctx ) e
-}


module TypeCheck where

import Control.Applicative
import State

type VarName = String
type Ctx = [(VarName, Type)]

type InferNum = Integer
type Facts = [(InferNum, Maybe Type)]

data Expr = EVar VarName 
          | ETypedAbs VarName Type Expr
          | EInferAbs VarName Expr 
          | EApp Expr Expr
          deriving (Show, Eq)

data Type = TVar String
          | TSimple String
          | TArrow Type Type
          | TInfer InferNum 
          deriving (Show, Eq)

type InferEngine = (InferNum, Facts)

nullEngine :: InferEngine
nullEngine = (0, [])

newInfer :: State InferEngine InferNum 
newInfer = State $ \ (inferNum, facts) -> ( (inferNum + 1, facts), inferNum )

failure :: State a (Maybe b)
failure = pure Nothing

typeof :: Ctx -> Expr -> Maybe Type
typeof ctx e = resolve $ evalFinalState (typeof' ctx e) nullEngine

resolve :: (InferEngine, Maybe Type) -> Maybe Type
resolve = undefined

-- unify for infer will add to the Facts which need to be resolved
-- unify for TVar will need to overwrite things in ctx or something (add new with same name but higher in list?)

typeof' :: Ctx -> Expr -> State InferEngine (Maybe Type)
typeof' ctx (EVar n) = pure $ lookup n ctx
typeof' ctx (ETypedAbs n t e) = typeof' ( (n, t) : ctx ) e
typeof' ctx (EInferAbs n e) = 
    do
        infer <- fmap TInfer newInfer
        typeof' ( (n, infer) : ctx ) e


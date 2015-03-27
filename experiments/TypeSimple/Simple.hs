
module Simple where

import Control.Applicative
import State

data Expr = EVar String
          | EAbs String Expr
          | EApp Expr Expr
    deriving (Show, Eq)

data Type = TSimple String
          | TArrow Type Type
          | TVar Integer
    deriving (Show, Eq)

type Env = [(String, Type)]

typeOf :: Env -> Expr -> Maybe Type
typeOf env expr = evalState (typeOf' env expr) 0
    where 
        typeOf' env (EVar varName) = pure $ lookup varName env
        typeOf' env (EAbs paramName expr) =
            do
                i <- newInteger
                exprType <- typeOf' ( (paramName, TVar i) : env ) expr
                return $ TArrow (TVar i) <$> exprType 

        typeOf' env (EApp e1 e2) = 
            do
                t1 <- typeOf' env e1
                t2 <- typeOf' env e2
                return $ mu (apply <$> t1 <*> t2)

apply :: Type -> Type -> Maybe Type
apply (TSimple _) _ = Nothing
apply (TVar _) _ = Nothing -- wrong (\ a -> \ b -> a b) ... we need to assign 'a' to Arrow (Var b) ? and then unify it with whatever 'a' ends up being in some later step?
apply (TArrow (TVar replace) outputType) inputType = Just $ substitute replace inputType outputType
apply (TArrow inputType outputType) inputType' 
    | inputType == inputType' = Just outputType
    | otherwise = Nothing


substitute :: Integer -> Type -> Type -> Type
substitute replaceThis withThis inThis =
    case inThis of
        t@(TSimple _) -> t
        TArrow t1 t2 -> TArrow (substitute replaceThis withThis t1) (substitute replaceThis withThis t2) 
        t@(TVar value) -> if value == replaceThis then withThis else t

mu :: Maybe (Maybe a) -> Maybe a
mu Nothing = Nothing
mu (Just Nothing) = Nothing
mu (Just a) = a

newInteger :: State Integer Integer
newInteger = State $ \ i -> (i + 1, i)


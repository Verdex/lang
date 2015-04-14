
module TypeUnifier where

import Control.Applicative
import Unify
import Struct
import State


-- this function really looks like it wants to merge my type environment and term environment monads
-- function, parameter, resulting type after application (or failure)
unifyApply :: Type -> Type -> State Integer (Maybe Type)
unifyApply (TSimple _) _ = pure Nothing -- cannot apply anything to an atom
unifyApply (TArrow expectedInput output) actualInput = 
    let res = Just $ typeToTerm output in
    let mEnv = unify [] (typeToTerm expectedInput) (typeToTerm actualInput) in
        pure $ termToType <$> (varReplace <$> mEnv <*> res)
unifyApply v@(TVar i) input =
    do
        newExpected <- fmap TVar newInteger
        newOutput <- fmap TVar newInteger
        let new = (TArrow newExpected newOutput) in
         let mEnv = unify [] (typeToTerm v) (typeToTerm new) in
          let res = Just $ typeToTerm newOutput in
           let mEnv' = mu $ unify <$> mEnv <*> Just (typeToTerm newExpected) <*> Just (typeToTerm input) in
            return $ termToType <$> (varReplace <$> mEnv' <*> res)
        

typeToTerm :: Type -> Term
typeToTerm (TSimple name) = Constant name
typeToTerm (TVar i) = Variable i 
typeToTerm (TArrow t1 t2) = Function "TArrow" [typeToTerm t1, typeToTerm t2]

termToType :: Term -> Type
termToType (Constant name) = TSimple name
termToType (Variable i) = TVar i
termToType (Function "TArrow" (t1:t2:[])) = TArrow (termToType t1) (termToType t2)


mu :: Maybe (Maybe a) -> Maybe a
mu Nothing = Nothing
mu (Just Nothing) = Nothing
mu (Just a) = a



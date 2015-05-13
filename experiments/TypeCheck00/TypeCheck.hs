
module TypeCheck where




data Expr = EVar String
          | EAbs String Expr -- optional type annotation?
          | EApp Expr Expr


data Type = TVar String
          | TSimple String
          | TArrow Type Type
          | TInfer Integer
          -- TApp ? or indexed or something

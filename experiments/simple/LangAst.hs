
module LangAst where

data Token = Symbol String
           | Match
           | With
           | Let 
           | In
           | End
           | Assign
           | Lambda
           | RArrow
           | Underscore
           | Colon
           | Semicolon
           | RParen
           | LParen
           | OrBar
           | Data
    deriving (Show, Eq)

data Program = Program [TopLevel]
    deriving (Show, Eq)

data TopLevel = Define ValueDef 
              | TypeDefine TypeDef 
    deriving (Show, Eq)

data Pattern = Pattern { pattern_cons :: String
                       , pattern_params :: [Pattern]
                       }
    deriving (Show, Eq)

data Expr = EVar String
          | ELet { let_name :: String
                 , let_assign :: Expr 
                 , let_body :: Expr
                 , let_sig :: TypeSig
                 }
          | EAbs { abs_params :: [String]
                 , abs_body :: Expr
                 }
          | EApp Expr Expr
          | EMat { match_target :: Expr
                 , match_case :: [(Pattern, Expr)]
                 }
    deriving (Show, Eq)

data ValueDef = ValueDef { valuedef_name :: String
                         , valuedef_expr :: Expr
                         , valuedef_sig :: TypeSig
                         }
    deriving (Show, Eq)

data Cons = Cons { cons_name :: String
                 , cons_params :: [TypeSig]
                 }
    deriving (Show, Eq)

data TypeDef = TypeDef { typedef_name :: String
                       , typedef_cons :: [Cons]
                       }
    deriving (Show, Eq)

data TypeSig = TSingle String
             | TArrow TypeSig TypeSig 
    deriving (Show, Eq)

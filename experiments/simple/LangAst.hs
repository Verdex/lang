
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

data TopLevel = Define Def 
              | TypeDefine TypeDef 
    deriving (Show, Eq)

data Pattern = Pattern { p_cons :: String
                       , p_param :: [Pattern]
                       }
             | DestructVariable String
    deriving (Show, Eq)

data MatchExpr = MatchExpr { m_target :: Expr
                           , m_case :: [(Pattern, Expr)]
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
          | EMat MatchExpr 
    deriving (Show, Eq)

data Def = Def { d_name :: String
               , d_expr :: Expr
               , d_sig :: TypeSig
               }
    deriving (Show, Eq)

data Cons = Cons { c_name :: String
                 , c_params :: [TypeSig]
                 }
    deriving (Show, Eq)

data TypeDef = TypeDef { td_name :: String
                       , td_cons :: [Cons]
                       }
    deriving (Show, Eq)

data TypeSig = TSingle String
             | TArrow TypeSig TypeSig 
    deriving (Show, Eq)

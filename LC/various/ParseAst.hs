
module ParseAst where


data Lambda =
    Var String
    | Paren Lambda
    | Abs String Lambda
    | App Lambda Lambda

    deriving (Show, Eq)



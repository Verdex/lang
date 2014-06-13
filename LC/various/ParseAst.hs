
module ParseAst where


data Lambda =
    Var String
    | Abs String Lambda
    | App Lambda Lambda

    deriving (Show, Eq)



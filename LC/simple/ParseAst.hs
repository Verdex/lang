
module ParseAst where


data Lambda =
    Var String
    | Abs String Lambda
    | App Lambda Lambda

    deriving (Show, Eq)


data Assignment = Assignment String Lambda 
    deriving (Show, Eq)

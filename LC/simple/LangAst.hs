
module LangAst where


-- instead of lambda term is probably a better choice
data Lambda =
    Var String
    | Abs String Lambda
    | App Lambda Lambda

    deriving (Show, Eq)


-- probably a better name for this than assignment (term def?)
data Assignment = Assignment String Lambda 

    deriving (Show, Eq)


data Type = 
    TypeName String
    | TypeArrow Type Type 
    
    deriving (Show, Eq)

data DataDef = DataDef String Type

    deriving (Show, Eq)

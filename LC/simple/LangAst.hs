
module LangAst where


data Lambda =
    Var String
    | Abs String Lambda
    | App Lambda Lambda

    deriving (Show, Eq)


-- probably a better name for this than assignment (term def?)
data Assignment = Assignment String Lambda 

    deriving (Show, Eq)

data Type = ? 

data DataDef = DataDef String 



data IntList = IntCons Int IntList | IntNil

data IntTree = IntLeaf Int | IntNode IntLeaf IntLeaf | IntNil

data IntPair = Right Int | Left Int

data Ikky = Ikky ( Int -> Int )

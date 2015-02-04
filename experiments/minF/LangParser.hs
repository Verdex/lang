
module LangParser (parse) where


import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined


typeDef :: Parser [Token] TypeDef
typeDef =
    do
        literally Type ()
        typeName <- anySymbol
        typeParams <- many anySymbol
        return $ TypeDef typeName typeParams

expr :: Parser [Token] Expr
expr = application
   <|> variable 
   <|> parenExpr
   <|> abstraction

parenExpr :: Parser [Token] Expr
parenExpr =
    do
        literally LParen ()
        e <- expr
        literally RParen ()
        return e

variable :: Parser [Token] Expr
variable = fmap EVar anySymbol

application :: Parser [Token] Expr
application = 
    do
        e <- allButApp
        es <- some allButApp
        return $ buildApp e es

    where allButApp = variable 
                  <|> parenExpr
                  <|> abstraction

          buildApp e1 (e2 : []) = EApp e1 e2
          buildApp e1 (e2 : es) = buildApp (EApp e1 e2) es

abstraction :: Parser [Token] Expr
abstraction =
    do
        literally Lambda ()
        params <- many anySymbol
        literally RArrow ()
        body <- expr
        return $ EAbs params body

anySymbol :: Parser [Token] String
anySymbol = matchSymbol ?=> projSymbol

    where matchSymbol (Symbol _) = True
          matchSymbol _ = False
          projSymbol (Symbol n) = n



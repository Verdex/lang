
module LangParser (parse) where


import Control.Applicative
import Parsing
import LangAst

-- left recursion problems exist because of the relationship between typesig and typeapp

parse :: [Token] -> Program
parse = undefined

typeSig :: Parser [Token] TypeSig
typeSig = typeArrow
      <|> typeApp
      <|> typeVar
      <|> parenType

typeVar :: Parser [Token] TypeSig
typeVar = fmap TSingle anySymbol

parenType :: Parser [Token] TypeSig
parenType =
    do
        literally LParen ()
        t <- typeSig 
        literally RParen ()
        return t

typeApp :: Parser [Token] TypeSig
typeApp =
    do
        t <- allButApp
        ts <- some allButApp
        return $ build t ts

    where allButApp = typeVar 
                  <|> parenType
                  -- <|> typeArrow 

          build t1 (t2 : []) = TApp t1 t2
          build t1 (t2 : ts) = build (TApp t1 t2) ts

typeArrow :: Parser [Token] TypeSig
typeArrow = 
    do
        t1 <- allButArrow
        literally RArrow ()
        t2 <- typeSig 
        return $ TArrow t1 t2

    where allButArrow = typeVar 
                    <|> parenType
                    <|> typeApp

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




module LangParser (parse) where


import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse tokens = case parseWith program (makeParseSource tokens) of
                    Success prog _ -> prog
                    Failure -> ParseError

program :: Parser [Token] Program
program = 
    do
        prog <- fmap Program $ many topLevel
        end
        return prog

topLevel :: Parser [Token] TopLevel
topLevel = fmap Define valueDef
        <|> fmap DefineSig valueSig
        <|> fmap TypeDefine typeDef

valueSig :: Parser [Token] ValueSig
valueSig =
    do
        valueSig <- anySymbol
        literally Colon ()
        t <- typeSig 
        literally Semicolon ()
        return $ ValueSig valueSig t

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
        t <- varAndParen 
        ts <- some varAndParen 
        return $ build t ts

    where varAndParen = typeVar 
                  <|> parenType

          build t1 (t2 : []) = TApp t1 t2
          build t1 (t2 : ts) = build (TApp t1 t2) ts

typeArrow :: Parser [Token] TypeSig
typeArrow = 
    do
        t1 <- allButArrow
        literally RArrow ()
        t2 <- typeSig 
        return $ TArrow t1 t2

    where allButArrow = typeApp
                    <|> typeVar 
                    <|> parenType

typeDef :: Parser [Token] TypeDef
typeDef =
    do
        literally Type ()
        typeName <- anySymbol
        typeParams <- many anySymbol
        literally Semicolon ()
        return $ TypeDef typeName typeParams

valueDef :: Parser [Token] ValueDef
valueDef =
    do
        valueName <- anySymbol
        literally Assign ()
        e <- expr
        literally Semicolon ()
        return $ ValueDef valueName e

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
        param <- anySymbol
        literally RArrow ()
        body <- expr
        return $ EAbs param body

anySymbol :: Parser [Token] String
anySymbol = matchSymbol ?=> projSymbol

    where matchSymbol (Symbol _) = True
          matchSymbol _ = False
          projSymbol (Symbol n) = n



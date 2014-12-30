
module LangParser where

-- TODO only export parse

import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined

-- NOTE:  Application needs to come before variable in order for "a (b c)" that case to 
-- parse correctly.  Because <|> is an ordered choice, if variable comes first then
-- we will consume the 'b' as a variable instead of the first expr in an application.
expr :: Parser [Token] Expr
expr = application
   <|> variable 
   <|> parenExpr
   <|> letExpr
   <|> matchExpr
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
                  <|> letExpr 
                  <|> matchExpr 
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
        return $ EAbs { abs_params = params
                      , abs_body = body
                      }

matchExpr :: Parser [Token] Expr
matchExpr =
    do
        literally Match ()
        target <- expr
        literally With ()
        cases <- some _case
        literally End ()
        return $ EMat { match_target = target
                      , match_cases = cases
                      }

    where _case = 
            do
                pat <- pattern
                literally RArrow ()
                e <- expr
                literally Semicolon ()
                return $ (pat, e)

pattern :: Parser [Token] Pattern
pattern = 
    do
        lparen <- zeroOrOne $ literally LParen ()
        consName <- anySymbol
        innerPatterns <- many $ (fmap (flip Pattern []) anySymbol)
                                <|> (lookAhead (literally LParen ()) >>= const pattern)
        rparen <- zeroOrOne $ literally RParen ()
        assert $ lparen == rparen
        return $ Pattern consName innerPatterns
                                
letExpr :: Parser [Token] Expr
letExpr = 
    do
        literally Let ()
        name <- anySymbol
        literally Colon ()
        sig <- typeSig
        literally Assign ()
        assign <- expr
        literally In ()
        body <- expr
        return $ ELet { let_name = name
                      , let_sig = sig
                      , let_assign = assign
                      , let_body = body
                      }


typeDef :: Parser [Token] TypeDef 
typeDef = 
    do
        literally Data ()
        name <- anySymbol
        literally Assign ()
        cs <- consList
        return $ TypeDef { typedef_name = name
                         , typedef_cons = cs
                         }

    where consDef =
            do
                consName <- anySymbol
                params <- many typeSig
                return $ Cons { cons_name = consName
                              , cons_params = params
                              }
          consListNode =
            do
                literally OrBar ()
                c <- consDef
                return c

          consList =
            do
                c <- zeroOrOne consDef
                cs <- many consListNode
                literally Semicolon ()
                return $ comb c cs

          comb (Just c) cs = c : cs
          comb Nothing cs = cs


typeSig :: Parser [Token] TypeSig
typeSig = arrow <|> paren <|> tsym 

    where tsym = 
            do
                name <- anySymbol
                return $ TSingle name
          arrow = 
            do 
                a <- paren <|> tsym 
                literally RArrow ()
                b <- typeSig
                return $ TArrow a b

          paren =
            do
                literally LParen ()
                t <- typeSig
                literally RParen ()
                return t


anySymbol :: Parser [Token] String
anySymbol = matchSymbol ?=> projSymbol

    where matchSymbol (Symbol _) = True
          matchSymbol _ = False
          projSymbol (Symbol n) = n



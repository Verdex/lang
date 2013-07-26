
module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import LangStuff

symbol :: Parser Char
-- very clever, haskell strings are just char arrays 
-- (feels like an impedance mismatch waiting to happen)
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
-- TODO:  clearly this cant be the final status of this function
-- normally i dont like case b/c of weird parsing issues that ive had in the past
-- although i dont like its syntactic appearence it looks (semantically) a lot like agda "with" 
-- which feels like a win (at least with does ... not sure about case...) 
readExpr input = case parse (spaces >> symbol) "" input of
    Left err -> "no match: " ++ show err
    Right val -> "Symbol!!"

spaces :: Parser ()
-- not sure why we aren't using the official 'spaces' function 
-- ... maybe they're trying to make this into a learning experience
spaces = skipMany1 space

parseString :: Parser LispVal
-- bleh, escaping " will probably suck (although honestly, raw strings are pretty awesome
-- so maybe I'll just skip the whole escapsing thing altogether and go for that instead)
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x




module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import LangStuff

symbol :: Parser Char
-- very clever, haskell strings are just char arrays 
-- (feels like an impedance mismatch waiting to happen)
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
    Left err -> "no match: " ++ show err
    Right val -> "found value"

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

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
-- cool, first timme Ive had need of liftM
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber

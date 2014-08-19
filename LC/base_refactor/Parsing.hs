
module Parsing where

import Control.Applicative
import Data.Char


type ParseString = (Int, String)

makeParseString :: String -> ParseString
makeParseString str = (0,str)

data ParseResult a = 
    Success a ParseString
    | Failure 

newtype Parser a = Parser( ParseString -> ParseResult a )

parseWith (Parser p) = p


instance Functor Parser where
    fmap f parser = Parser $
        \ ps -> case parseWith parser ps of
            Success a ps' -> Success (f a) ps'
            Failure -> Failure

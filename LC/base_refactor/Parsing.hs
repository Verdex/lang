
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

instance Applicative Parser where
    pure a = Parser $ \ ps -> Success a ps
    parser1 <*> parser2 = Parser $
        \ ps -> case parseWith parser1 ps of
            Failure -> Failure
            Success f ps' -> case parseWith parser2 ps' of
                Failure -> Failure
                Success a ps'' -> Success (f a) ps''


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

instance Monad Parser where
    return = pure
    parser >>= gen = Parser $
        \ ps -> case parseWith parser ps of
            Failure -> Failure
            Success a ps' -> parseWith (gen a) ps'

instance Alternative Parser where
    empty = Parser $ \ ps -> Failure
    parser1 <|> parser2 = Parser $ 
        \ ps -> case parseWith parser1 ps of
            Success a ps' -> Success a ps'
            Failure -> case parseWith parser2 ps of
                Success a ps' -> Success a ps'
                Failure -> Failure
    many parser = Parser $ 
        \ ps -> case parseWith parser ps of
            Failure -> Success [] ps
            Success a ps' -> case parseWith (many parser) ps' of
                Failure -> Success [a] ps'
                Success as ps'' -> Success (a : as) ps''
    some parser = 
        do
            a <- parser
            as <- many parser
            return (a : as)

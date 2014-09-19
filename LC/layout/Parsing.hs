
module Parsing where

import Control.Applicative
import Data.Char

x = \ a -> a
x' = \ a -> a 5
x'' = \ a -> a
    5
x''' = \ a ->
    a 5

y = \ a 
    -> a 5

y' = \
    a -> a 5

y'' = 
    \ a -> a 5

type ParseSource = (Int, String)

makeParseSource :: String -> ParseSource
makeParseSource str = (0,str)

data ParseResult a = 
    Success a ParseSource
    | Failure 
    
    deriving Show

data Parser a = Parser (ParseSource -> ParseResult a)

parseWith (Parser p) = p

instance Functor Parser where

    fmap f parser = Parser $
        \ ps -> 
            case parseWith parser ps 
            of
                Success a ps' -> Success (f a) ps'
                Failure -> Failure 


instance Applicative Parser where

    pure a = Parser $ \ ps -> Success a ps

    parser1 <*> parser2 = Parser $
        \ ps -> 
            case parseWith parser1 ps 
            of
                Failure -> Failure
                Success f ps' -> 
                    case parseWith parser2 ps' 
                    of
                        Failure -> Failure
                        Success a ps'' -> Success (f a) ps''


instance Monad Parser where

    return = pure

    parser >>= gen = Parser $
        \ ps -> 
            case parseWith parser ps 
            of
                Failure -> Failure 
                Success a ps' -> parseWith (gen a) ps'


instance Alternative Parser where

    empty = Parser $ \ ps -> Failure 

    parser1 <|> parser2 = Parser $ 
        \ ps -> 
            case parseWith parser1 ps 
            of
                Success a ps' -> Success a ps'
                Failure -> 
                    case parseWith parser2 ps
                    of
                        Success a ps' -> Success a ps'
                        Failure -> Failure 

    many parser = Parser $ 
        \ ps -> 
            case parseWith parser ps 
            of
                Failure -> Success [] ps
                Success a ps' -> 
                    case parseWith (many parser) ps' 
                    of
                        Failure -> Success [a] ps'
                        Success as ps'' -> Success (a : as) ps''

    some parser = 
        do
            a <- parser 
            as <- many parser
            return (a : as)


end = Parser $ 
    \ ps@(i, s) -> 
        case i == length s 
        of
            True -> Success () ps
            False -> Failure


getAnyX matcher transform = Parser $
    \ (i,s) -> 
        case i >= length s
        of
            True -> Failure 
            False -> 
                let x = s !! i 
                in
                    case matcher x 
                    of
                        True -> Success (transform x) (i+1, s)
                        False -> Failure


getAnyDigit = (getAnyX isDigit digitToInt) 

getAnyAlpha = (getAnyX isLetter id) 

getString str = Parser $
    \ (i,s) -> 
        let len = length str 
        in
            case str == take len (drop i s)
            of
                True -> Success str (i+len,s)
                False -> Failure

lookAhead parser = Parser $ 
    \ ps -> 
        case parseWith parser ps
        of
            Success a ps' -> Success a ps
            f@Failure -> f

assert False = Parser $ \ ps -> Failure
assert True = Parser $ \ ps -> Success () ps
        


module Parsing where

import Control.Applicative
import Data.Char


type ParseString = (Int, String)

makeParseString :: String -> ParseString
makeParseString str = (0,str)

data ParseResult a = 
    Success a ParseString
    | Failure 
    
    deriving Show

data Parser a = Parser [String] (ParseString -> ParseResult a)

anonParser = Parser [] 

parseWith (Parser _ p) = p


instance Functor Parser where

    fmap f parser = anonParser $
        \ ps -> 
            case parseWith parser ps 
            of
                Success a ps' -> Success (f a) ps'
                Failure -> Failure


instance Applicative Parser where

    pure a = anonParser $ \ ps -> Success a ps

    parser1 <*> parser2 = anonParser $
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

    parser >>= gen = anonParser $
        \ ps -> 
            case parseWith parser ps 
            of
                Failure -> Failure
                Success a ps' -> parseWith (gen a) ps'


instance Alternative Parser where

    empty = anonParser $ \ ps -> Failure

    parser1 <|> parser2 = anonParser $ 
        \ ps -> 
            case parseWith parser1 ps 
            of
                Success a ps' -> Success a ps'
                Failure -> 
                    case parseWith parser2 ps
                    of
                        Success a ps' -> Success a ps'
                        Failure -> Failure

    many parser = anonParser $ 
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


end = anonParser $ 
    \ ps@(i, s) -> 
        case i == length s 
        of
            True -> Success () ps
            False -> Failure 


getAnyX matcher transform = anonParser $
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

getAnyDigit = getAnyX isDigit digitToInt

getWhiteSpace = getAnyX isSpace id

getAnyAlpha = getAnyX isLetter id

getString str = anonParser $
    \ (i,s) -> 
        let len = length str 
        in
            case str == take len (drop i s)
            of
                True -> Success str (i+len,s)
                False -> Failure

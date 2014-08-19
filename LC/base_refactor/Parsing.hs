
module Parsing where

import Control.Applicative
import Data.Char


type ParseString = (Int, String)

makeParseString :: String -> ParseString
makeParseString str = (0,str)

data ParseResult a = 
    Success a ParseString
    | Failure [String]
    
    deriving Show

data Parser a = Parser (Maybe String) (ParseString -> ParseResult a)

anonParser = Parser Nothing 
namedParser = (Parser . Just)

parseWith (Parser _ p) = p

withError (Parser Nothing _) = "Anon"
withError (Parser (Just e) _) = e

(Parser _ p) <?> error = Parser (Just error) p

instance Functor Parser where

    fmap f parser = namedParser "fmap" $
        \ ps -> 
            case parseWith parser ps 
            of
                Success a ps' -> Success (f a) ps'
                Failure es -> Failure $ (withError parser) : es


instance Applicative Parser where

    pure a = namedParser "pure" $ \ ps -> Success a ps

    parser1 <*> parser2 = namedParser "<*>" $
        \ ps -> 
            case parseWith parser1 ps 
            of
                Failure _ -> Failure [] 
                Success f ps' -> 
                    case parseWith parser2 ps' 
                    of
                        Failure _ -> Failure [] 
                        Success a ps'' -> Success (f a) ps''


instance Monad Parser where

    return = pure

    parser >>= gen = namedParser "Bind" $
        \ ps -> 
            case parseWith parser ps 
            of
                Failure es -> Failure $ (withError parser) : es
                Success a ps' -> parseWith (gen a) ps'


instance Alternative Parser where

    empty = namedParser "empty" $ \ ps -> Failure []

    parser1 <|> parser2 = namedParser "<|>" $ 
        \ ps -> 
            case parseWith parser1 ps 
            of
                Success a ps' -> Success a ps'
                Failure errors1 -> 
                    case parseWith parser2 ps
                    of
                        Success a ps' -> Success a ps'
                        Failure errors2 -> Failure ["Left:" ++ (show ((withError parser1) : errors1)) ++ ";" ++
                                                    "Right:" ++ (show ((withError parser2) : errors2))]

    many parser = anonParser $ 
        \ ps -> 
            case parseWith parser ps 
            of
                Failure _ -> Success [] ps
                Success a ps' -> 
                    case parseWith (many parser) ps' 
                    of
                        Failure _ -> Success [a] ps'
                        Success as ps'' -> Success (a : as) ps''

    some parser = 
        do
            a <- parser 
            as <- many parser
            return (a : as)


end = namedParser "end of stream" $ 
    \ ps@(i, s) -> 
        case i == length s 
        of
            True -> Success () ps
            False -> Failure []


getAnyX matcher transform = anonParser $
    \ (i,s) -> 
        case i >= length s
        of
            True -> Failure []
            False -> 
                let x = s !! i 
                in
                    case matcher x 
                    of
                        True -> Success (transform x) (i+1, s)
                        False -> Failure []

getAnyDigit = (getAnyX isDigit digitToInt) <?> "getAnyDigit"

getWhiteSpace = (getAnyX isSpace id) <?> "getWhiteSpace"

getAnyAlpha = (getAnyX isLetter id) <?> "getAnyAlpha"

getString str = namedParser ("getString:" ++ str) $
    \ (i,s) -> 
        let len = length str 
        in
            case str == take len (drop i s)
            of
                True -> Success str (i+len,s)
                False -> Failure []

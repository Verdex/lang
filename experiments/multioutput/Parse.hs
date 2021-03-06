

module Parse where

import Control.Applicative
import Data.Char

type ParseString = (Int, String)

newtype Parser a = Parser( ParseString -> ( Maybe a, ParseString ) )

makeParseString :: String -> ParseString
makeParseString s = (0,s) 
parse (Parser p) = p

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ s -> case p s of
        (Nothing, output) -> (Nothing, output)
        (Just a, output) -> (Just $ f a, output)

instance Applicative Parser where
    pure a = Parser $ \ s -> (Just a, s)
    p1 <*> p2 = Parser $ \ s -> case parse p1 s of
        (Nothing, output1) -> (Nothing, output1)
        (Just a, output1) -> case parse p2 output1 of
            (Nothing, output2) -> (Nothing, output2)
            (Just b, output2) -> (Just $ a b, output2)

instance Monad Parser where
    return = pure
    p >>= gen = Parser $ \ s -> case parse p s of
        (Nothing, output) -> (Nothing, output)
        (Just a, output) ->  parse (gen a) output

instance Alternative Parser where
    empty = Parser $ \ s -> (Nothing, s)
    p1 <|> p2 = Parser $ \ s -> case parse p1 s of
        success@(Just a, output) -> success
        (Nothing, _) -> parse p2 s
    -- many is zero or more
    many p = Parser $ \ s -> case parse p s of
        (Nothing, output) -> (Just [], output)
        (Just a, output) -> let (mas, o2) = parse (many p) output in
            case mas of
                Nothing -> (Just [a], o2)
                Just as -> (Just $ a : as, o2) 
    -- some is one or more
    some p = do 
                f <- p
                r <- many p
                return (f : r)

getString match = Parser $ \ (i, s) -> let l = length match in
    case match == take l (drop i s) of
        True -> (Just match, (i + l, s))
        False -> (Nothing, (i, s) ) 

lookAhead p = Parser $ \ ps@(i, s) -> 
    case parse p ps of
        (Just a, _) -> (Just a, ps) 
        r@(Nothing, _) -> r

endStream = Parser $ \ ps@(i, s) -> 
    case i == length s of
        True -> (Just (), ps)
        False -> (Nothing, ps)

getAnyX recog trans = Parser $ \ ps@(i, s) ->
    case i >= length s of
        True -> (Nothing, ps )
        False ->
            let x = s !! i in
                case recog x of 
                    True -> (Just $ trans x, (i + 1, s))
                    False -> (Nothing, ps)

getAnyDigit = getAnyX isDigit digitToInt
getWhiteSpace = getAnyX isSpace id
getAnyLetter = getAnyX isLetter id 

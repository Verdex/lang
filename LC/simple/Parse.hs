

module Parse where

import Control.Applicative
import Data.Char

type ParseString = (Int, String)

data ParseResult a =
    Success a ParseString
    | Failure ParseString 

newtype Parser a = Parser( ParseString -> ParseResult a )

makeParseString :: String -> ParseString
makeParseString s = (0,s) 
parse (Parser p) = p

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ ps -> case p ps of
        Failure _ -> Failure ps
        Success a ps' -> Success (f a) ps' 

instance Applicative Parser where
    pure a = Parser $ \ ps -> Success a ps 
    p1 <*> p2 = Parser $ \ ps -> case parse p1 ps of
        Failure _ -> Failure ps
        Success a ps' -> case parse p2 ps' of
            Failure _ -> Failure ps -- the second one fails means the first one fails?  (what ps to use?)
            Success b ps'' -> Success (a b) ps''

instance Monad Parser where
    return = pure
    p >>= gen = Parser $ \ ps -> case parse p ps of
        Failure _ -> Failure ps
        Success a ps' -> parse (gen a) ps'

instance Alternative Parser where
    empty = Parser $ \ ps -> Failure ps
    p1 <|> p2 = Parser $ \ ps -> case parse p1 ps of
        Success a ps' -> Success a ps'
        Failure _ -> parse p2 ps
    -- many is zero or more
    many p = Parser $ \ ps -> case parse p ps of
        Failure _ -> Success [] ps
        Success a ps' -> case re ps' of
            Failure _ -> Success [a] ps'
            Success as ps'' -> Success (a : as) ps''

        where
            re ps''' = parse (many p) ps'''
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


module Parsing where

import Control.Applicative


type ParseSource a = (Int, a)

makeParseSource :: a -> ParseSource a
makeParseSource as = (0,as)

data ParseResult s a = 
    Success a (ParseSource s)
    | Failure 
    
    deriving Show

data Parser s a = Parser (ParseSource s -> ParseResult s a)

parseWith (Parser p) = p


instance Functor (Parser s) where
    fmap f parser = Parser $ \ ps -> handleResult $ parseWith parser ps
    
        where handleResult (Success a ps) = Success (f a) ps 
              handleResult Failure = Failure


instance Applicative (Parser s) where
    pure a = Parser $ \ ps -> Success a ps
    
    parser1 <*> parser2 = 
        do
            f <- parser1
            a <- parser2
            return $ f a


instance Monad (Parser s) where
    return = pure

    parser >>= gen = mu $ fmap gen parser
    
        where mu parser = Parser $ \ ps -> inner $ parseWith parser ps
              inner (Success p ps) = parseWith p ps
              inner Failure = Failure


instance Alternative (Parser s) where
    empty = Parser $ \ ps -> Failure 

    parser1 <|> parser2 = Parser $ \ ps -> (tryAgainOnFailure ps) $ parseWith parser1 ps

        where tryAgainOnFailure ps Failure = parseWith parser2 ps
              tryAgainOnFailure _ s@(Success a ps) = s 
    
    many parser = Parser $ \ ps -> initialParse ps $ parseWith parser ps 

        where initialParse ps Failure = Success [] ps
              initialParse _ (Success a ps) = subsequentParse ps a $ parseWith (many parser) ps 

              subsequentParse ps a Failure = Success [a] ps
              subsequentParse _ a (Success as ps) = Success (a : as) ps

    some parser = 
        do
            a <- parser 
            as <- many parser
            return (a : as)


-- Ensures that you have reached the end of the input being parsed.
end :: Parser [s] ()
end = Parser $ \ ps@(i, s) -> 
    if i == length s then
        Success () ps
    else
        Failure

-- If the matcher indicates a match then we use the
-- transformer to convert the value into a result.
-- The value is consumed.
(?=>) :: (a -> Bool) -> (a -> b) -> Parser [a] b
(?=>) matcher transform = Parser $ \ (i,s) -> 
    if i >= length s then
        Failure
    else 
        let x = s !! i 
        in
            if matcher x then
                Success (transform x) (i+1, s)
            else
                Failure

literally :: Eq a => a -> b -> Parser [a] b
literally target result = (target ==) ?=> (const result)

-- Parses a string and then produces some result.
stringBecomes :: String -> a -> Parser String a
stringBecomes str result = Parser $ \ (i,s) -> 
    let len = length str 
    in
        if str == take len (drop i s) then
            Success result (i+len, s)
        else
            Failure

-- Does not consume input for successful parse.
lookAhead :: Parser s a -> Parser s a
lookAhead parser = Parser $ 
    \ ps -> 
        case parseWith parser ps
        of
            Success a ps' -> Success a ps
            f@Failure -> f

-- Will successfully parse zero or one instance.
zeroOrOne :: Parser s a -> Parser s (Maybe a)
zeroOrOne parser = Parser $ 
    \ ps -> 
        case parseWith parser ps
        of 
            Success a ps' -> Success (Just a) ps'
            Failure -> Success Nothing ps
            
-- Parses 'a' until a 'b' or non 'a' is encountered
-- non 'a' is not consumed, 'b' is not consumed
-- may return a zero length list
parseUntil :: Parser s a -> Parser s b -> Parser s [a]
parseUntil pa pb = checkForPb <|> getPas
    
    where checkForPb = fmap (const []) $ lookAhead pb 
          getPas = 
            do 
                fst <- pa
                rst <- zeroOrOne $ parseUntil pa pb
                return $ fst : (fromMaybe rst)

          fromMaybe (Just a) = a
          fromMaybe Nothing = []

assert :: Bool -> Parser s ()
assert False = Parser $ \ ps -> Failure
assert True = Parser $ \ ps -> Success () ps


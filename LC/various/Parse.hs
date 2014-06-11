

module Parse where

import Control.Applicative

type ParseString = (Int, String)

newtype Parser a = Parser( ParseString -> ( Maybe a, ParseString ) )

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ s -> case p s of
        (Nothing, output) -> (Nothing, output)
        (Just a, output) -> (Just $ f a, output)

instance Applicative Parser where
    pure a = Parser $ \ s -> (Just a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \ s -> case p1 s of
        (Nothing, output1) -> (Nothing, output1)
        (Just a, output1) -> case p2 output1 of
            (Nothing, output2) -> (Nothing, output2)
            (Just b, output2) -> (Just $ a b, output2)

instance Monad Parser where
    return = pure
    (Parser p) >>= gen = Parser $ \ s -> case p s of
        (Nothing, output) -> (Nothing, output)
        (Just a, output) -> ((\ (Parser n) -> n) $ gen a) output

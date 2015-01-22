
module Tokenizer (tokenize) where


import Control.Applicative
import Data.List
import Data.Char
import Parsing
import LangAst


tokenize :: String -> [Token]
tokenize str = 
    case parseWith allTokens (makeParseSource str) 
    of
        Success ts _ -> mapReduce ts
        Failure -> []
    where convert (Just t) = [t]
          convert Nothing = []

          reduce = foldl' (++) []

          mapReduce = reduce . (map convert)
          

allTokens =
    do
        ts <- many $ squashSpaces
                  <|> squashNewLine  
                  <|> squashComment
                  <|> "="     `strToTok` LangAst.Assign
                  <|> "\\"    `strToTok` LangAst.Lambda
                  <|> "->"    `strToTok` LangAst.RArrow
                  <|> ":"     `strToTok` LangAst.Colon
                  <|> ";"     `strToTok` LangAst.Semicolon
                  <|> "("     `strToTok` LangAst.LParen
                  <|> ")"     `strToTok` LangAst.RParen
                  <|> "type"  `strToTok` LangAst.Type
                  <|> symbol
        end
        return ts


symbol :: Parser String (Maybe Token)
symbol = fmap (Just . LangAst.Symbol) $ some (isLetter ?=> id) 

strToTok :: String -> Token -> Parser String (Maybe Token)
strToTok s t = fmap Just $ s `stringBecomes` t

squashSimple :: String -> Parser String (Maybe Token)
squashSimple s = fmap (const Nothing) $ s `stringBecomes` () 

squashNewLine :: Parser String (Maybe Token)
squashNewLine = (squashSimple "\r\n") <|> (squashSimple "\r") <|> (squashSimple "\n")

squashSpaces :: Parser String (Maybe Token)
squashSpaces = squashSimple " " 

squashComment = 
    do
        "--" `stringBecomes` ()
        -- This line says parse everything and destroy until we hit a new line.
        ((const True) ?=> (const ())) `parseUntil` squashNewLine
        return Nothing


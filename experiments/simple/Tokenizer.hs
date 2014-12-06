
module Tokenizer where

import Control.Applicative
import Data.List
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
                  <|> (getSimple "_" LangAst.Underscore)
                  <|> (getSimple "match" LangAst.Match)
                  <|> (getSimple "with" LangAst.With)
                  <|> (getSimple "let" LangAst.Let)
                  <|> (getSimple "in" LangAst.In)
                  <|> (getSimple "end" LangAst.End)
                  <|> (getSimple "=" LangAst.Assign)
                  <|> (getSimple "\\" LangAst.Lambda)
                  <|> (getSimple "->" LangAst.RArrow)
                  <|> (getSimple ":" LangAst.Colon)
                  <|> (getSimple ";" LangAst.Semicolon)
                  <|> (getSimple "(" LangAst.LParen)
                  <|> (getSimple ")" LangAst.RParen)
                  <|> (getSimple "|" LangAst.OrBar)
                  <|> (getSimple "data" LangAst.Data)
                  <|> getSymbol
        end
        return ts


getSymbol = fmap (Just . LangAst.Symbol) $ some getAnyAlpha

getSimple :: String -> Token -> Parser String (Maybe Token)
getSimple s t =
    do 
        getString s
        return $ Just t

squashSimple :: String -> Parser String (Maybe Token)
squashSimple s = 
    do
        getString s
        return Nothing 

squashNewLine :: Parser String (Maybe Token)
squashNewLine = (squashSimple "\r\n") <|> (squashSimple "\r") <|> (squashSimple "\n")

squashSpaces :: Parser String (Maybe Token)
squashSpaces = squashSimple " " 

squashComment = 
    do
        getString "--"
        parseUntil (getAnyX (const True) (const ())) squashNewLine
        return Nothing

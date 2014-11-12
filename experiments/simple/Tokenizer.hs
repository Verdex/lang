
module Tokenizer where

import Control.Applicative
import Data.List
import Parsing
import LangAst


tokenize :: String -> [Token]
tokenize str = 
    case parseWith allTokens (makeParseSource str) 
    of
        Success ts _ -> jabber (map blah ts)
        Failure -> []
    where blah (Just t) = [t]
          blah Nothing = []

          jabber = foldl' (++) []

allTokens =
    do
        ts <- many $ squashSpaces
                  <|> squashNewLine  
                  <|> getSymbol
        end
        return ts

getSymbol = fmap (Just . LangAst.Symbol) $ some getAnyAlpha

underscore = getSimple "_"

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




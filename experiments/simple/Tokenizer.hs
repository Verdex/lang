
module Tokenizer where

import Control.Applicative
import Parsing
import LangAst


tokenize :: String -> [Token]
tokenize str = 
    case parseWith allTokens (makeParseSource str) 
    of
        Success ts _ -> ts
        Failure -> []

allTokens =
    do
        ts <- many $ getSpaces
                  <|> getNewLine  
                  <|> getSymbol
        end
        return ts

getSymbol = some getAnyAlpha

-- this
underscore = fmap (const LangAst.Underscore) (getString "_")


getSimple :: String -> Token -> Parser String Token
getSimple s t =
    do 
        getString s
        return t

squashSimple :: String -> Parser String ()
squashSimple = undefined

-- squash end line
getNewLine = undefined  --(getSimple "\r\n" NewLine) <|> (getSimple "\r" NewLine) <|> (getSimple "\n" NewLine)

-- squash spaces
getSpaces = fmap (const ()) $ some $ getString " "




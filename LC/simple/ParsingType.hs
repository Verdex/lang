
module ParsingType where

import Control.Applicative
import Parsing
import ParsingUtils
import LangAst



getTypeName = fmap TypeName getTypeSymbol

--getTypeParen = withParens ?typeParser

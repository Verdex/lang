
module LangParser where

-- TODO only export parse

import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined


-- TODO the tree structure AST I have built up might end up 
-- kind of sucking b/c I'm going to need to either have a
-- tree structure parser (that might happen otherwise so
-- whatever) or I might need a lot of weird trivial mapings set up.
-- Actually, the tree structures of other parser ASTs I've seen
-- before seemed kind of unnerving (granted the structure
-- kind of comes with the territory ...).  I wonder if there's a
-- way to work that out better.

pData :: [Token] -> TypeDef 


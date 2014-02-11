
module( ..., package.seeall )


--[[

looks like pythons syntax works b/c of colons at
the end of a line

def blah:

def blah a b c :

def blah
    a 
    b
    c:

def blah 
    a b
    c:

def blah a b c:

might be able to pull off haskell syntax by staying
in pure mode ... if basically you only have one function
application (with a bunch of sub function applications) 
maybe that works out okay for the parser

let blah = 

blah = 

blah a b c = 

it looks like haskell lexing works similar to python

let blah = ...
    blah2 = ...
in ...

case x of 
    1 -> blah
    2 -> blah

blah z          -- this one is a bit weird ... for everything else it looks like line it up is the rule, but with where maybe everything past the first directly after where needs to line up of course it might also be that let works the same way but just happens to be 4 characters long(let + space)
    where blah 0 = 0
        blah 1 = 1
        blah 2 = 2 

multiline stuff seems "intuitive"

let blah = 
     2
     * 3 * 4
    blah2 = 
     2 * 3 * 4

just need a single space of indentation and it's a multiline


goal is no unnecessary parens, no commas, no end character



I've heard it said that python's lexer is context sensitive while its parser is 
context free.  I'm not about to try and break out the parsing theory, but the upshot
that seems to effect me is that layout syntax means I'm going to need to lex in 
implicit open and close brackets that would have otherwise shown up as part of
the explicit syntax.

Python style seems relatively straight forward.  Always have a : before
you open the "block".

Haskell style seems to suggest more or less the same thing as python except
there's a lot of different analogs for the :.  let blah *=* or case x *of*
--]]

--[[
    input string
    ouput array lexeme
--]]
function lex( str )

    

end

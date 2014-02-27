
--module( ..., package.seeall ) -- TODO uncomment

require "lexeme"

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


local function consumeAll( cons, target, str, index )
    local count = 0
    local c = string.sub( str, index, index ) 
    while string.match( c, target ) do
        count = count + 1
        index = index + 1
        c = string.sub( str, index, index )
    end

    return cons( count ), index 
end

local function matchString( key )
    return function ( str )
        return key == str 
    end
end

local function identifier( str )
    -- TODO expand this to work with underscore and any other 
    -- symbols that I want in identifiers
    return string.match( str, "%w+" )
end

local function number( str )
    -- right now only get generic integers working
    -- worry about the full spectrum of numbers later
    return string.match( str, "%d+" )
end

local function cEntry( matcher, conser )
    return { m = matcher; c = conser }
end

local cTable = {
    cEntry( matchString "(", lexeme.mk_leftParen ),
    cEntry( matchString ")", lexeme.mk_rightParen ),
    cEntry( matchString "def", lexeme.mk_def ),
    cEntry( number, lexeme.mk_number ), 
    -- identifier will match a lot of keywords as well, 
    -- make sure to place it last
    cEntry( identifier, lexeme.mk_identifier ), 
}

--[[
    Converts a string into a lexeme
--]]
local function convert( str )
    for _, v in ipairs( cTable ) do
        if v.m( str ) then
            return v.c( str )
        end
    end
end

--[[
    input string
    ouput array lexeme
--]]
function lex( str )

    local ls = {}

    local i = 1
    local j = 1
    while i <= #str do
        local c = string.sub( str, i, i )
        -- %c characters count as %s characters so %c needs to
        -- be checked first
        if string.match( c, "%c" ) then 
            local wordLex = convert( string.sub( str, j, i - 1 ) )
            local endLex, n = consumeAll( lexeme.mk_endline, "%c", str, i ) 
            ls[#ls+1] = wordLex
            ls[#ls+1] = endLex
            i = n
            j = i 
        elseif string.match( c, "%s" ) then
            local wordLex = convert( string.sub( str, j, i - 1 ) )
            local spaceLex, n = consumeAll( lexeme.mk_space, "%s", str, i )
            ls[#ls+1] = wordLex
            ls[#ls+1] = spaceLex 
            i = n
            j = i 
        else
            i = i + 1
        end
    end

    if j ~= i then
        local wordLex = convert( string.sub( str, j, i ) )
        ls[#ls+1] = wordLex 
    end

    return ls

end



--[[ 
go until you see whitespace ... depending on what state we're in
we're going to do something different about the whitespace
--]]


--[[
once the space algorithm stuff is out of the way (part one is in lex right now
and part two will be adding scope data to the lexeme array) it looks a lot 
like mk_*, is_*, and match_* (right now the match functions are just spread
around in this file) are all related to each other.  There's probably a better
way to structure this stuff (and maybe even a way to largely abstract lexers)
... I'm not sure if that just looks like using a lex generator (somehow
I'm not sure that it does because they are missing good ways to abstract
within their input), but I'll see when I finish the space/scoping stuff
--]]

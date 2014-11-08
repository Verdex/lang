
require "data"
require "parse"

module( ..., package.seeall )

local mk_string = parse.mk_string
local bind = parse.bind
local unit = parse.unit
local endStream = parse.endStream
local alternative = parse.alternative
local zeroOrMore = parse.zeroOrMore
local oneOrMore = parse.oneOrMore
local get_string = parse.get_string
local get_anyDigit = parse.get_anyDigit
local get_anyLetter = parse.get_anyLetter
local get_whiteSpace = parse.get_whiteSpace

function get_paren( str )
    return bind( get_string "(", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function ( expr )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_string ")", function ()
    return unit( data.mk_paren( expr ) )
    end ) end ) end ) end ) end )( str )
end

-- parser variable 
function get_variable( str )
    return bind( alternative{ get_string "_", get_anyLetter }, function ( first )
    return bind( zeroOrMore( alternative{ get_string "_", get_anyLetter, get_anyDigit } ), function ( rest )
    table.insert( rest, 1, first )
    return unit( data.mk_variable( table.concat( rest ) ) ) end ) end )( str )
end

-- parser abstraction
function get_abstraction( str )
    return bind( get_string "\\", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    -- Not sure I really want the parameter to be a variable instead of a string
    -- you get strange artifacts like abs.name.name in the code.
    -- I'm going to leave it for now, but the next revision should probably 
    -- address this better.
    return bind( get_variable, function ( variableName )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_string ".", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function ( term )
    return unit( data.mk_abstraction( variableName, term ) ) 
    end ) end ) end ) end ) end ) end ) end )( str )
end

local function buildApplication( f, vs, i )
    if #vs == i then
        return data.mk_application( f, vs[i] )
    end
    return buildApplication( data.mk_application( f, vs[i] ), vs, i + 1 )
end

local function get_applicationHelp( str )
    return bind( oneOrMore( get_whiteSpace ), function ()
    return bind( alternative{ get_variable, get_abstraction, get_paren }, function ( value )
    return unit( value )
    end ) end )( str )
end

function get_application( str )
    return bind( alternative{ get_variable, get_abstraction, get_paren }, function ( func )
    return bind( oneOrMore( get_applicationHelp ), function ( values )
    return unit( buildApplication( func, values, 1 ) )
    end ) end )( str )
end

function get_lambdaTerm( str )
    return alternative{ get_application, get_variable, get_abstraction, get_paren }( str )
end

function get_assignment( str )
    -- consider get_string instead of get_variable for next revision
    return bind( get_variable, function ( name )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_string "=", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function( expr )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    -- think i can get rid of the ';' by using a lookahead for '<blah> ='
    -- alternatively, i can also add in a let form and do lookahead on 'let'
    -- finally, i can do the haskell / python thing
    -- for now I just want it working so i can figure out a more modular arch
    return bind( get_string ";", function ()
    return unit( data.mk_assignment( name, expr ) )
    end ) end ) end ) end ) end ) end ) end )( str )
end

local function whiteItemWhite( item )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( item, function( i )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return unit( i )
    end ) end ) end )
end

function get_lambdaCalculus( str )
    return bind( zeroOrMore( whiteItemWhite( get_assignment ) ), function ( assignments )
    return bind( endStream, function ()
    return unit( assignments )
    end ) end )( str )
end



require "lang"

module( ..., package.seeall )

--[[

\-term =
    | ( \-term )
    | var
    | \ var . \-term
    | \-term \-term 

var = (_|char) .. (_|char|digit)*

parser a : string -> ( bool, a, string )
       | string -> false, nil, string
       | string -> true, a, string

--]]

function mk_string( str, index )
    index = index or 1
    return { str = str, index = index }
end

-- parser a -> (a -> parser b) -> parser b 
function bind( parser, action ) 
    return function ( str )
        local success, result, str2 = parser( str )
        if not success then
            return false, nil, str
        end
        return action( result )( str2 )
    end
end

-- a -> parser a
function unit( a )
    return function ( str )
        return true, a, str
    end
end

-- parser nil
function endStream( str )
    if str.index == string.len( str.str ) + 1 then
        return true, nil, str
    end
    return false, nil, str
end

-- [parser ?] -> parser ?
function alternative( ps )
    return function ( str )
        for _, p in ipairs( ps ) do
            local success, result, str2 = p( str )
            if success then
                return true, result, str2
            end
        end
        return false, nil, str 
    end
end

-- parser a -> parser [a]
function zeroOrMore( p )
    return function ( str )
        local results = {}
        repeat 
            local success, result, str2 = p( str )
            if not success then
                return true, results, str
            end
            results[#results + 1] = result
            str = str2
        until false 
    end
end

-- parser a -> parser [a]
function oneOrMore( p )
    return bind( p, function ( first ) 
    return bind( zeroOrMore( p ), function ( rest ) 
    table.insert( rest, 1, first ) 
    return unit( rest ) end ) end )
end

-- string -> parser string
function get_string( value )
    return function( str )
        local l = string.len( value )
        local m = string.sub( str.str, str.index, str.index + l - 1 )
        if m == value then
            return true, value, mk_string( str.str, str.index + l )
        end
        return false, nil, str
    end
end

-- parser number
function get_anyDigit( str )
    local sub = string.sub( str.str, str.index, str.index )
    local match = string.match( sub, "%d" ) 
    if match then
        return true, tonumber( match ), mk_string( str.str, str.index + 1 )
    end
    return false, nil, str
end

-- parser string
function get_anyLetter( str )
    local sub = string.sub( str.str, str.index, str.index )
    local match = string.match( sub, "%a" ) 
    if match then
        return true, match, mk_string( str.str, str.index + 1 )
    end
    return false, nil, str
end

-- parser string
function get_whiteSpace( str )
    local sub = string.sub( str.str, str.index, str.index )
    local match = string.match( sub, "%s" ) 
    if match then
        return true, match, mk_string( str.str, str.index + 1 )
    end
    return false, nil, str
end

function get_paren( str )
    return bind( get_string "(", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function ( expr )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_string ")", function ()
    return unit( lang.mk_paren( expr ) )
    end ) end ) end ) end ) end )( str )
end

-- parser variable 
function get_variable( str )
    return bind( alternative{ get_string "_", get_anyLetter }, function ( first )
    return bind( zeroOrMore( alternative{ get_string "_", get_anyLetter, get_anyDigit } ), function ( rest )
    table.insert( rest, 1, first )
    return unit( lang.mk_variable( table.concat( rest ) ) ) end ) end )( str )
end

-- parser abstraction
function get_abstraction( str )
    return bind( get_string "\\", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_variable, function ( variableName )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_string ".", function ()
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function ( term )
    return unit( lang.mk_abstraction( variableName, term ) ) 
    end ) end ) end ) end ) end ) end ) end )( str )
end

function get_applicationHelp( func )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( get_lambdaTerm, function ( value )
    return unit( lang.mk_application( func, value ) )
    end ) end )
end

function get_application( str )
    return bind( alternative{ get_variable, get_abstraction, get_paren }, get_applicationHelp )( str )
end

function get_lambdaTerm( str )
    return alternative{ get_application, get_variable, get_abstraction, get_paren }( str )
end

function whiteItemWhiteEnd( item )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( item, function( i )
    return bind( zeroOrMore( get_whiteSpace ), function ()
    return bind( endStream, function () 
    return unit( i )
    end ) end ) end ) end )
end

function get_lambdaCalculus( str )
    return alternative{
        whiteItemWhiteEnd( get_application ),
        whiteItemWhiteEnd( get_variable ),
        whiteItemWhiteEnd( get_abstraction ),
        whiteItemWhiteEnd( get_paren ) }( str )
end

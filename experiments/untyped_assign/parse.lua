
module( ..., package.seeall )

--[[ 

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


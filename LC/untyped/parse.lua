
--module( ..., package.seeall )

--[[

\-term =
    | ( \-term )
    | var
    | \ var . \-term
    | \-term \-term 

parser a : string -> ( bool, a, string )
       | string -> false, nil, string
       | string -> true, a, string

--]]


function mk_string( str, index )
    index = index or 1
    return { str = str, index = index }
end

-- parser string
function get_string( value )
    return function( str )
        local l = string.len( value )
        local m = string.sub( str.str, str.index, str.index + l - 1 )
        print( l, m )
        if m == value then
            return true, value, mk_string( str.str, str.index + l )
        end
        return false, nil, str
    end
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

function alternative( parsers )
     
end





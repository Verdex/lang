
module( ..., package.seeall )

function mk_def()
    return { name = "def" }
end

function mk_identifier( str )
    return { name = "identifier"; value = str }
end

function mk_endline()
    return { name = "endline" } 
end

function mk_space( space_count )
    return { name = "space"; count = space_count }
end

local function is( str )
    return function( lexeme )
        return 
            type( lexeme ) == "table"
            and lexeme.name == str 
    end
end

is_def = is "def" 
is_endline = is "endline"
is_space = is "space"
is_identifier = is "identifier"

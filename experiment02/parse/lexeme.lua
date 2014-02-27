
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

function mk_leftParen()
    return { name = "leftParen" }
end

function mk_rightParen()
    return { name = "rightParen" }
end

function mk_number( num )
    -- only one number type for now
    -- if my more ambitious ideas work out I'll worry about making
    -- the full spectrum of numbers parseable later
    return { name = "number"; value = tonumber( num ) }
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
is_number = is "number"
is_leftParen = is "leftParen"
is_rightParen = is "rightParen"

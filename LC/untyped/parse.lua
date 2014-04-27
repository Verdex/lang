
--module( ..., package.seeall )

--[[

\-term =
    | ( \-term )
    | var
    | \ var . \-term
    | \-term \-term 

parser : string -> ([AST] | failure, string) 
       : string -> bool, [AST], string
       : string -> false
       : string -> true, [AST], string

--]]


local function mk_string( str, index )
    index = index or 1
    return { str = str, index = index }
end

-- match that takes my string and runs the lua pattern engine given a pattern
-- maybe just want simple string compare ... anyway create and return new 
-- my string with new index (use recursion as backtrace to kill the bad indices on failure)

-- [parser] -> parser
function compose( pasers ) 
    return function ( str )
        local asts = {}
        for parser in ipairs( parsers ) do
            local success, ast
            success, ast, str = parser( string ) -- string being assigned is different?
            if not success then
                return false
            end
            asts[#asts + 1] = ast
        end
        return true, asts, str
    end
end

function eat_nothing_success( str )

end

function eat_nothing_fail( str )
    return false
end

local function alternative( parsers )
     
end





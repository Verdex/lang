
module( ..., package.seeall )

function mk_def()
    return { name = "def" }
end

function mk_endline()
    return { name = "endline" } 
end

function mk_space( space_count )
    return { name = "space"; count = space_count }
end


function is_def( lexeme )
    if type( lexeme ) == "table" then
        return lexeme.name == "def"
    end
end


function is_endline( lexeme )
    if type( lexeme ) == "table" then
        return lexeme.name == "endline"
    end
end

function is_space( lexeme )
    if type( lexeme ) == "table" then
        return lexeme.name == "space"
    end
end


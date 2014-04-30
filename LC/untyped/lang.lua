
module( ..., package.seeall )

function mk_abstraction( var, expr )
    return { type = "abs", var = var, expr = expr }
end

function mk_variable( name )
    return { type = "var", name = name }
end

local function is( t )
    return function ( expr )
        return expr.type == t
    end
end

is_abstraction = is "abs"
is_variable = is "var"

function eq( a, b )
    if a.type ~= b.type then
        return false
    end

    if is_abstraction( a ) then
        if not eq( a.var, b.var ) then
            return false
        end
        return eq( a.expr, b.expr )
    end

    if is_variable( a ) then
        return a.name == b.name
    end

end


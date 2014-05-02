
    
module( ..., package.seeall )

function mk_abstraction( var, expr )
    return { type = "abs", var = var, expr = expr }
end

function mk_variable( name )
    return { type = "var", name = name }
end

function mk_application( func, value )
    return { type = "app", func = func, value = value }
end

function mk_paren( expr )
    return { type = "paren", expr = expr }
end

local function is( t )
    return function ( expr )
        return expr.type == t
    end
end

is_abstraction = is "abs"
is_variable = is "var"
is_application = is "app"
is_paren = is "paren"

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
    
    if is_application( a ) then
        return eq( a.func, b.func ) and eq( a.value, b.value )
    end

    if is_paren( a ) then
        return eq( a.expr, b.expr )
    end

    return false
end


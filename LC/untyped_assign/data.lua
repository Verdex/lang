
module( ..., package.seeall )

function mk_assignment( name, expr )
    return { type = "assign", name = name, expr = expr }
end

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

is_assignment = is "assign"
is_abstraction = is "abs"
is_variable = is "var"
is_application = is "app"
is_paren = is "paren"

function eq( a, b )
    if a.type ~= b.type then
        return false
    end

    if is_assignment( a ) then 
        if not eq( a.name, b.name ) then
            return false
        end
        return eq( a.expr, b.expr )
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

function stringify( expr )

    if is_assignment( expr ) then
        return stringify( expr.name ) .. " = " .. stringify( expr.expr )
    end
    
    if is_paren( expr ) then
        return "( " .. stringify( expr.expr ) .. " )"
    end
    
    if is_abstraction( expr ) then
        return "\\ " .. stringify( expr.var ) .. " . " .. stringify( expr.expr )
    end

    if is_application( expr ) then
        return stringify( expr.func ) .. " " .. stringify( expr.value )
    end

    if is_variable( expr ) then
        return expr.name
    end

    return nil
end

function stringify_annotate( expr, depth )
    depth = depth or 0

    if is_assignment( expr ) then
        return string.rep( " ", depth ) .. "ASSIGN: " .. expr.name.name
            .. " =\n" .. stringify_annotate( expr.expr, depth + 1 )
    end

    if is_paren( expr ) then
        return string.rep( " ", depth ) .. "PAREN\n" .. stringify_annotate( expr.expr, depth + 1 )
    end
    
    if is_abstraction( expr ) then
        return string.rep( " ", depth ) .. "ABS: " .. expr.var.name .. "\n" 
            .. stringify_annotate( expr.expr, depth + 1 )
    end

    if is_application( expr ) then
        return string.rep( " ", depth ) .. "APP\n" .. stringify_annotate( expr.func, depth + 1 )
            .. "\n" .. stringify_annotate( expr.value, depth + 1 ) 
    end

    if is_variable( expr ) then
        return string.rep( " ", depth ) .. "VAR: " .. expr.name
    end

    return nil
end



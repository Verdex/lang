
require "data"

module( ..., package.seeall )

local function compileHelp( expr, env )
    if data.is_paren( expr ) then
        return "( " .. compileHelp( expr.expr, env ) .. " )"
    end
    
    if data.is_abstraction( expr ) then
        local param = expr.var.name
        env[param] = (env[param] or 0) + 1 
        local ret = "function ( " .. param .. " ) return " .. compileHelp( expr.expr, env ) .. " end"
        env[param] = env[param] - 1
        return ret
    end

    if data.is_application( expr ) then
        return "( " .. compileHelp( expr.func, env ) .. " )( " .. compileHelp( expr.value, env ) .. " )"
        
    end

    if data.is_variable( expr ) then
        if env[expr.name] and env[expr.name] > 0 then
            return expr.name
        else
            return "topLevel[\"" .. expr.name .. "\"]"
        end
    end
end

-- [assignment] -> [lua func]
function compile( assignments )
    local proto = {}
    for _, assignment in ipairs( assignments ) do
        local res, err = load( "return function( topLevel ) return " 
                                .. compileHelp( assignment.expr, {} ) .. " end" )
        if not res then
            return nil, err
        end
        proto[#proto + 1] = { n = assignment.name.name, r = res() }
    end
    local funcs = {}
    for _, p in ipairs( proto ) do
        funcs[p.n] = p.r( funcs )
    end
    return funcs
end


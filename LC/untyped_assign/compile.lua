
require "data"

module( ..., package.seeall )

-- compile to final output

local function luaifyHelp( expr )
    if is_paren( expr ) then
        return "( " .. luaifyHelp( expr.expr ) .. " )"
    end
    
    if is_abstraction( expr ) then
        return "function ( " .. luaifyHelp( expr.var ) .. " ) return " .. luaifyHelp( expr.expr ) .. " end"
    end

    if is_application( expr ) then
        return "( " .. luaifyHelp( expr.func ) .. " )( " .. luaifyHelp( expr.value ) .. " )"
        
    end

    if is_variable( expr ) then
        return expr.name
    end

    return nil
end

function luaify( expr ) -- TODO probably want to change this name (at least whatever the interface is going to be)
    return loadstring( "return " .. luaifyHelp( expr ) )()
end



require "data"

module( ..., package.seeall )

-- [assignment] -> bool
function allNamesAreUnique( assignments )
    local names = {}
    for _, assignment in ipairs( assignments ) do
        if names[assignment.name.name] then
            return false
        end
        names[assignment.name.name] = true
    end
    return true 
end

--[[
the final step is a translation into lua code
long term goals will probably be to mangle and demangle
the variable and assignment names so that they do not 
conflict with lua reserved words but still give meaningful
error messages when things go poorly.

This revision will just enforce lua's reserved word list though.
--]]
local reservedWords = {
    ["and"] = true, 
    ["break"] = true, 
    ["do"] = true, 
    ["else"] = true, 
    ["elseif"] = true, 
    ["end"] = true,
    ["false"] = true, 
    ["for"] = true, 
    ["function"] = true, 
    ["goto"] = true, 
    ["if"] = true, 
    ["in"] = true,
    ["local"] = true, 
    ["nil"] = true, 
    ["not"] = true, 
    ["or"] = true, 
    ["repeat"] = true, 
    ["return"] = true,
    ["then"] = true, 
    ["true"] = true, 
    ["until"] = true, 
    ["while"] = true,
}

--[[
This is an example of a function I really don't like.
There's "communication" going on between the recursive
calls of this function via the mutable reference to vars.
In this case nothing's really expected to go wrong. 
I would be worried if parallel eval was happening.
--]]
local function var_hunter( ast, vars )
    if is_assignment( a ) then 
        vars[#vars + 1] = a.name
        return var_hunter( a.expr, vars )
    end
    
    if is_abstraction( a ) then
        vars = var_hunter( a.var, vars )
        return var_hunter( a.expr, vars )
    end

    if is_variable( a ) then
        vars[#vars + 1] = a
        return vars 
    end
    
    if is_application( a ) then
        vars = var_hunter( a.func, vars )
        return var_hunter( a.value, vars )
    end

    if is_paren( a ) then
        return var_hunter( a.expr, vars )
    end
end

function noLuaReservedWords( assignments )
    for _, assignment in ipairs( assignments ) do
        local vars = var_hunter( assignment, {} )
        for _, var in ipairs( vars ) do
            if reservedWords[var.name] then
                return false
            end
        end
    end
    return true
end



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

function noLuaReservedWords( assignments )
    for _, assignment in ipairs( assignments ) do
        
    end
end



require "data"

module( ..., package.seeall )

-- [assignment] -> bool
local function allAssignmentsAreUnique( assignments )
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
    if data.is_assignment( ast ) then 
        vars[#vars + 1] = ast.name
        return var_hunter( ast.expr, vars )
    end
    
    if data.is_abstraction( ast ) then
        vars = var_hunter( ast.var, vars )
        return var_hunter( ast.expr, vars )
    end

    if data.is_variable( ast ) then
        vars[#vars + 1] = ast
        return vars 
    end
    
    if data.is_application( ast ) then
        vars = var_hunter( ast.func, vars )
        return var_hunter( ast.value, vars )
    end

    if data.is_paren( ast ) then
        return var_hunter( ast.expr, vars )
    end
end

local function noLuaKeywords( assignments )
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

local function boundHelper( ast, topLevel, env ) -- : free variable name 
    if data.is_abstraction( ast ) then
        local n = ast.var.name
        if env[n] then
            env[n] = env[n] + 1
        else
            env[n] = 1
        end

        local r = boundHelper( ast.expr, topLevel, env )
        
        env[n] = env[n] - 1

        return r
    end

    if data.is_variable( ast ) then
        if not topLevel[ast.name] and (not env[ast.name] or env[ast.name] <= 0 ) then
            return ast.name
        else
            return ""
        end
    end
    
    if data.is_application( ast ) then
        return boundHelper( ast.func, topLevel, env ) and 
            boundHelper( ast.value, topLevel, env )
    end

    if data.is_paren( ast ) then
        return boundHelper( ast.expr, topLevel, env )
    end
end 

local function allVariablesAreBound( assignments )
    local topLevel = {}
    for _, assignment in ipairs( assignments ) do
        topLevel[assignment.name.name] = true
    end
    for _, assignment in ipairs( assignments ) do
        local value = boundHelper( assignment.expr, topLevel, {} )
        if value ~= "" then
            return false,  "unbound variable encountered:  " .. value 
                .. "  In top level assignment:  " .. assignment.name.name
        end
    end
    return true
end

function check( assignments )
    if not allAssignmentsAreUnique( assignments ) then
        return false, "all assigned names must be unique"
    end
    if not noLuaKeywords( assignments ) then
        return false, "you cannot use a lua keyword as an assignment or variable name"
    end
    local r, m = allVariablesAreBound( assignments ) 
    if not r then
        return false, m
    end
    return true
 
end

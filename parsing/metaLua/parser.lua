
--module( ..., package.seeall )

local function label( l, c )
    return { name = l, content = c }
end


local function separateRules( rules )
    local sr = {}
    local quote = false

    for i = 1, #rules do
        local c = rules:sub( i, i )

end

function parse( str )
    local name, rules = str:match( "^%s-meta%s-(%w+).-{(.-)}%s*$" )
    if name == nil or rules == nil then
        return false, "failure matching name and rules"
    end
    return name, rules
end



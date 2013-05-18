
--module( ..., package.seeall )

local function label( l, c )
    return { name = l, content = c }
end


-- bleh, let's go ad hoc and say \\ => \ and \' => '
-- also, i hate everything this function represents 
-- except the ability to get something working faster 
-- than a saner way (guess no movie this afternoon after all
-- ... making this not suck really isn't a priority because
-- i can always just replace it with the finished parser)
local function separateRules( rules )
    local rs = {} -- danger explicit accumulator in for loop
    local r = {}  -- danger list that gets stored and replaced in some loop iterations
    local quote = false -- danger flags that change and affect loop iterations
    local escape = false
    
    -- so the only reason im doing a bunch of crazy work with quote and escape is because
    -- i want to be able to separate the rules without prematurely separating a rule out
    -- because the input has a quoted comma

    for i = 1, #rules do
        local c = rules:sub( i, i )
        -- escape next character
        if c == [[\]] then 
            print( c )
            escape = true 
            goto continue -- oh good, the set a variable and continue pattern never causes disasters
        end
       
        -- try to escape
        if escape then 
            if c == "'"  or c == [[\]] then
                r[#r+1] = c
                escape = false
                goto continue
            else
                error "bad escape sequence"
            end
        end

        -- activate quote mode
        if c == "'" then
            quote = not quote
            goto continue
        end

        -- always eat character when in quote mode 
        if quote then
            r[#r+1] = c
            goto continue
        end
      
        -- found un-quoted comma, we've finished the current rule
        if c == ',' then
            rs[#rs+1] = r -- bleh
            r = {}
            goto continue
        end

        -- most cases
        r[#r+1] = c

        ::continue::
    end

    -- the last rule may or may not have a comma ending it
    if #r ~= 0 then
        rs[#rs+1] = r
    end

    local ret = {}
    for _, v in ipairs( rs ) do
        ret[#ret+1] = table.concat( v ) 
    end

    return ret
end

local function parseRuleDefQuote( a, def, i )
    return parseRuleDefKillWhiteSpace( a, def, i )
end

local function parseRuleDefKillWhiteSpace( a, def, i )
    local n = i + 1
    local c = def:sub( n, n )
    if c:match "%s"  or c == "\n" or c == "\r" then
        return parseRuleDefKillWhiteSpace( a, def, n )
    elseif c == "'" then
        return nil
    end
end


local function parseRuleDefStart( def )
     
end

local function parseRule( rule )
    local lhs, rhs = rule:match( "^.-(%w+).-=(.-)$" )  
    local ruleList = {}
    for m in rhs:gmatch( "(%g)*" ) do
        ruleList[#ruleList+1] = m
    end
    return lhs, ruleList 
end

function parse( str )
    local name, rules = str:match( "^%s-meta%s-(%w+).-{(.-)}.*$" )
    if name == nil or rules == nil then
        return false, "failure matching name and rules"
    end
    local srules = separateRules( rules )
    local prules = {}
    for _, r in ipairs( srules ) do
        local n, rs = parseRule( r )
        prules[#prules+1] = { n = n, rules = rs } 
    end
    return name, prules 
end



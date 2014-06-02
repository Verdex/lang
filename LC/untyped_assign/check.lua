
require "data"

module( ..., package.seeall )

-- [assignment] -> bool
function allNamesAreUnique( assignments )
    local names = {}
    for _, assignment in ipairs( assignments ) do
        if names[assignment.name] then
            return false
        end
        names[assignment.name] = true
    end
    return true 
end

-- this file is going to error check the program (long term goals include type checker et al)



require "lang"
require "parse"

module( ..., package.seeall )

function create_lambda( str )
    local s = parse.mk_string( str )
    local suc, res, _ = parse.get_lambdaCalculus( s )

    if not suc then
        print "here"
        return false
    end
    
    return lang.luaify( res )
end


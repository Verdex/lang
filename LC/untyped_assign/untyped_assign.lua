
require "data"
require "parse"

module( ..., package.seeall )

function compile_lambda( str )
    --local s = parse.mk_string( str )
    --local suc, res, _ = parse.get_lambdaCalculus( s )

    if not suc then
        return false
    end
    
    return data.luaify( res )
end


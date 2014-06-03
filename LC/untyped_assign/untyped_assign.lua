
require "parse"
require "lang_parse"
require "check"
require "compile"

module( ..., package.seeall )

function eval( str )
    local s = parse.mk_string( str )
    local suc, res, s = lang_parse.get_lambdaCalculus( s )
    if not suc then
        return false, "parse fails near index " .. s.index
    end
    local suc, mes = check.check( res )
    if not suc then
        return false, mes
    end

    local funcs, mes = compile.compile( res )
    if not funcs then
        return false, mes
    end
    
    return funcs
end


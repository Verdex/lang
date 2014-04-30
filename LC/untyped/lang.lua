
module( ..., package.seeall )

function mk_abstraction( var, expr )
    return { type = "abs", var = var, expr = expr }
end

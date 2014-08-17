
dofile "output.lua"

function eval_bool( b ) 
    return b( true )( false )
end

function eval_num( n )
    return n( function ( v ) return v + 1 end )( 0 ) 
end

funcs = create()

-- The factorial of 5 is 120
suc = funcs.suc
zero = funcs.zero
assert( eval_num( funcs.factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) == 120 )

print(  eval_num( funcs.factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) )


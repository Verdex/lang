
require "untyped_assign"

local compile_lambda = untyped_assign.compile_lambda

id_prog = compile_lambda [[

id = \ i . i

main = id

]]

local t1 = {}
assert( id_prog( t1 ) == t1 )




--[=[
zero = compile_lambda[[\ f . \ v . v]]
suc = compile_lambda[[\ n . \ f . \ v . f ( n f v )]]
pred = compile_lambda[[ \n . \ f . \ v . n ( \ a . \ b . b ( a f ) ) ( \ k . v ) ( \ i . i )]]
add = compile_lambda([[\ suc . \ n1 . \ n2 . n1 suc n2]])( suc )
sub = compile_lambda([[\ pred . \ n1 . \ n2 . n1 pred n2]])( pred )
mult = compile_lambda([[\ zero . \ add . \ n1 . \ n2 . n1 (add n2) zero]])( zero )( add )
true_val = compile_lambda[[\ x . \ y . x]]
false_val = compile_lambda[[\ x . \ y . y]]
is_zero = compile_lambda(
    [[\ true_val . \ false_val . \ n . n ( \ x . false_val ) true_val]])( true_val )( false_val )

fac = compile_lambda(
    [[\ is_zero . \ suc . \ pred . \ mult . \ fac . \ num .
        is_zero 
            num 
            ( \ v . suc num v ) 
            ( \ v . mult num ( fac ( pred num ) ) v )]])( is_zero )( suc )( pred )( mult )

Y = compile_lambda[[\ f . ( \ x . f ( \ y . x x y ) ) ( \ x . f ( \ y . x x y ) )]]

function eval_bool( b ) 
    return b( true )( false )
end

function eval_num( n )
    return n( function ( v ) return v + 1 end )( 0 ) 
end


-- The factorial of 5 is 120
assert( eval_num( Y( fac )( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) == 120 )

--]=]


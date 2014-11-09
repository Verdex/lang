
require "untyped"

zero = untyped.create_lambda[[\ f . \ v . v]]
suc = untyped.create_lambda[[\ n . \ f . \ v . f ( n f v )]]
pred = untyped.create_lambda[[ \n . \ f . \ v . n ( \ a . \ b . b ( a f ) ) ( \ k . v ) ( \ i . i )]]
add = untyped.create_lambda([[\ suc . \ n1 . \ n2 . n1 suc n2]])( suc )
sub = untyped.create_lambda([[\ pred . \ n1 . \ n2 . n1 pred n2]])( pred )
mult = untyped.create_lambda([[\ zero . \ add . \ n1 . \ n2 . n1 (add n2) zero]])( zero )( add )
true_val = untyped.create_lambda[[\ x . \ y . x]]
false_val = untyped.create_lambda[[\ x . \ y . y]]
is_zero = untyped.create_lambda(
    [[\ true_val . \ false_val . \ n . n ( \ x . false_val ) true_val]])( true_val )( false_val )

fac = untyped.create_lambda(
    [[\ is_zero . \ suc . \ pred . \ mult . \ fac . \ num .
        is_zero 
            num 
            ( \ v . suc num v ) 
            ( \ v . mult num ( fac ( pred num ) ) v )]])( is_zero )( suc )( pred )( mult )

Y = untyped.create_lambda[[\ f . ( \ x . f ( \ y . x x y ) ) ( \ x . f ( \ y . x x y ) )]]

function eval_bool( b ) 
    return b( true )( false )
end

function eval_num( n )
    return n( function ( v ) return v + 1 end )( 0 ) 
end


-- The factorial of 5 is 120
assert( eval_num( Y( fac )( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) == 120 )

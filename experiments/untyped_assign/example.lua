
require "untyped_assign"

funcs = untyped_assign.eval[[
zero = \ f . \ v . v ; 
suc = \ n . \ f . \ v . f ( n f v ) ;
pred = \n . \ f . \ v . n ( \ a . \ b . b ( a f ) ) ( \ k . v ) ( \ i . i ) ;
add = \ n1 . \ n2 . n1 suc n2 ;
sub = \ n1 . \ n2 . n1 pred n2 ;
mult = \ n1 . \ n2 . n1 (add n2) zero ;
true_val = \ x . \ y . x ;
false_val = \ x . \ y . y ;
is_zero = \ n . n ( \ x . false_val ) true_val ;

fac =  \ fac . \ num .
        is_zero 
            num 
            ( \ v . suc num v ) 
            ( \ v . mult num ( fac ( pred num ) ) v ) ;

Y = \ f . ( \ x . f ( \ y . x x y ) ) ( \ x . f ( \ y . x x y ) ) ;

factorial = Y fac ;

]]

function eval_bool( b ) 
    return b( true )( false )
end

function eval_num( n )
    return n( function ( v ) return v + 1 end )( 0 ) 
end


-- The factorial of 5 is 120
suc = funcs.suc
zero = funcs.zero
assert( eval_num( funcs.factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) == 120 )

print(  eval_num( funcs.factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) )


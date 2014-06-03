
require "untyped_assign"
require "data"
require "lang_parse"
require "parse"
require "check"

--[=[local compile_lambda = untyped_assign.compile_lambda

id_prog = compile_lambda [[

id = \ i . i

main = id

]]

local t1 = {}
assert( id_prog( t1 ) == t1 )

--]=]

str1 = parse.mk_string [[ blah = \ a . a ;]]
str2 = parse.mk_string [[ blah = \ a . a a ;]]
str3 = parse.mk_string [[ blah = a a ;]]
str4 = parse.mk_string [[ blah = a ;]]
str5 = parse.mk_string [[ blah = (\ a . a a) ( \ b . b b ) ; ]]
str6 = parse.mk_string [[ blah = \ a . \ b . b a ; ]]

_,pexpr1,_ = lang_parse.get_lambdaCalculus( str1 )
_,pexpr2,_ = lang_parse.get_lambdaCalculus( str2 )
_,pexpr3,_ = lang_parse.get_lambdaCalculus( str3 )
_,pexpr4,_ = lang_parse.get_lambdaCalculus( str4 )
_,pexpr5,_ = lang_parse.get_lambdaCalculus( str5 )
_,pexpr6,_ = lang_parse.get_lambdaCalculus( str6 )

expr1 = pexpr1[1]
expr2 = pexpr2[1]
expr3 = pexpr3[1]
expr4 = pexpr4[1]
expr5 = pexpr5[1]
expr6 = pexpr6[1]

assert( data.eq( expr1, expr1 ) )
assert( data.eq( expr2, expr2 ) )
assert( data.eq( expr3, expr3 ) )
assert( data.eq( expr4, expr4 ) )
assert( data.eq( expr5, expr5 ) )
assert( data.eq( expr6, expr6 ) )

print( data.stringify( expr1 ) )
print( data.stringify( expr2 ) )
print( data.stringify( expr3 ) )
print( data.stringify( expr4 ) )
print( data.stringify( expr5 ) )
print( data.stringify( expr6 ) )

print( data.stringify_annotate( expr1 ) )
print( data.stringify_annotate( expr2 ) )
print( data.stringify_annotate( expr3 ) )
print( data.stringify_annotate( expr4 ) )
print( data.stringify_annotate( expr5 ) )
print( data.stringify_annotate( expr6 ) )


str7 = parse.mk_string [[ 

blah = \ a . a;
jabber = \ b . \ a . a b

;

ikky = (\ j . j) (\ y . y)     ;    



wocky = 
    ( \ w . w w ) ( \ w . w w );


]]


_,pexpr7,_ = lang_parse.get_lambdaCalculus( str7 )

assert( pexpr7 ~= nil )

for _, v in ipairs( pexpr7 ) do
    print( data.stringify( v ) )
    print( data.stringify_annotate( v ) )
end

assert( check.check( pexpr7 ) )


str8 = parse.mk_string [[
    blah = \ a . a ;
    blah   = \ b . b b ;
]]

_,pexpr8,_ = lang_parse.get_lambdaCalculus( str8 )
assert( pexpr8 )
r8, m8 = check.check( pexpr8 )
assert( not r8 )
assert( m8 == "all assigned names must be unique" )

str9 = parse.mk_string [[
    blah = ( \ a . a ) ( \ goto . goto ) ; 
]]

_,pexpr9,_ = lang_parse.get_lambdaCalculus( str9 )
assert( pexpr9 )
r9, m9 = check.check( pexpr9 )
assert( not r9 )
assert( m9 == "you cannot use a lua keyword as an assignment or variable name" )

str10 = parse.mk_string [[
    blah = ( \ a . a ) ( \ a . b ) ; 
]]

_,pexpr10,_ = lang_parse.get_lambdaCalculus( str10 )
assert( pexpr10 )
r10, m10 = check.check( pexpr10 )
assert( not r10 )
assert( m10 == "unbound variable encountered:  b  In top level assignment:  blah" )


str11 = parse.mk_string [[
    b = \ a . a ;
    blah = ( \ a . a ) ( \ a . b ) ; 
    ikky = \ a . \ d . \ c . (\ c . c a d) c ; 
]]

_,pexpr11,_ = lang_parse.get_lambdaCalculus( str11 )
assert( pexpr11 )
assert( check.check( pexpr11 ) )

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


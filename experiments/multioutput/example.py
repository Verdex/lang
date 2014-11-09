
import output

def eval_bool( b ):
    return b( true )( false )

def eval_num( n ):
    return n( lambda v : v + 1 )( 0 ) 

funcs = output.create()

# The factorial of 5 is 120
suc = funcs['suc']
zero = funcs['zero']
factorial = funcs['factorial']

assert( eval_num( factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) == 120 )

print(  eval_num( factorial( suc( suc( suc( suc( suc( zero ) ) ) ) ) ) ) )


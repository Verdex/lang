
load "output.rb"

def eval_bool b 
    b[ true ][ false ]
end

def eval_num n 
    n[ -> v { v + 1 } ][ 0 ]
end

funcs = create

# The factorial of 5 is 120
suc = funcs['suc']
zero = funcs['zero']
factorial = funcs['factorial']

raise "failure" unless eval_num( factorial[ suc[ suc[ suc[ suc[ suc[ zero ] ] ] ] ] ] ) == 120 

puts( eval_num( factorial[ suc[ suc[ suc[ suc[ suc[ zero ] ] ] ] ] ] ) )


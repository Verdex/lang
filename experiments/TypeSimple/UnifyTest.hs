
import Unify


a = unify' [] (Constant "a") 
              (Constant "b")

b = unify' [] (Constant "a") 
              (Constant "a")

c = unify' [] (Function "a" []) 
              (Function "b" [])

d = unify' [] (Function "a" []) 
              (Function "a" [])

e = unify' [] (Function "a" [Constant "c"]) 
              (Function "a" [])

f = unify' [] (Function "a" [Constant "c"])
              (Function "a" [Constant "c"])

g = unify' [] (Function "a" [Constant "c", Constant "d"]) 
              (Function "a" [Constant "c", Constant "d"])

h = unify' [] (Function "a" [Constant "c", Constant "d"]) 
              (Function "a" [Constant "c", Constant "e"])

i = unify' [] (Function "a" [Function "b" [Constant "c"]])
              (Function "a" [Function "b" [Constant "c"]])

j = unify' [] (Function "a" [Function "b" [Constant "c"]])
              (Function "a" [Function "b" [Constant "d"]])
              
k = unify' [] (Function "a" [Function "b" [Constant "c"]])
              (Function "a" [Function "c" [Constant "c"]])

l = unify' [] (Variable 1) 
              (Variable 2)

m = unify' [(1, Constant "blah")] (Variable 1) 
                                  (Variable 2)

n = unify' [(2, Constant "blah")] (Variable 1) 
                                  (Variable 2)

o = unify' [(1, Function "x" [Constant "y"]), 
            (2, Function "x" [Constant "y"])] (Variable 1) 
                                              (Variable 2)

p = unify' [(1, Variable 3),
            (2, Variable 4)] (Variable 1)
                             (Variable 2)

q = unify' [(1, Variable 2),
            (2, Variable 1)] (Variable 1) -- this causes infinite loop
                             (Variable 2)

r = unify' [] (Variable 1)
              (Variable 1) -- this doesn't start an infinit loop, but it does setup the env to have (1, Variable 1)
                           -- which will case an infinite loop if unify (Var 1) (Var 1) ever occurs

s = unify' [(1, Variable 1)] (Variable 1) -- this causes infinite loop
                             (Variable 1)

t = unify' [(3, Constant "a"), 
            (1, Variable 3)] (Variable 1)
                             (Variable 2)

u = unify' [(3, Constant "a"), 
            (1, Variable 3)] (Variable 1)
                             (Constant "b") -- this will need to return Nothing

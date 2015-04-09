
import Unify

works (Just _) = True
works _ = False
fails Nothing = True
fails (Just _) = False

contains (i, t) (Just env) = 
    case lookup i env of
        Just t' -> t == t'
        Nothing -> False
contains _ Nothing = False

freeOf envlet menv = not $ contains envlet menv

(<+>) :: (Maybe Env -> Bool) -> (Maybe Env -> Bool) -> (Maybe Env -> Bool)
(<+>) a b menv = a menv && b menv

test :: (Maybe Env, (Maybe Env -> Bool), String) -> IO ()
test (result, test, name) 
    | test result = print ("Pass: " ++ name)
    | otherwise = print ("Fail: " ++ name)


main = 
    do
        test (unify [] (Constant "a") 
                       (Constant "b"), fails, "constant mismatch")
    
        test (unify [] (Constant "a") 
                       (Constant "a"), works, "constant match")

        test (unify [] (Function "a" []) 
                       (Function "b" []), fails, "function name mismatch")

        test (unify [] (Function "a" []) 
                       (Function "a" []), works, "function name match, no args")

        test (unify [] (Function "a" [Constant "c"]) 
                       (Function "a" []), fails, "function name match, arity mismatch")

        test (unify [] (Function "a" [Constant "c"])
                       (Function "a" [Constant "c"]), works, "function name match, args match")

        test (unify [] (Function "a" [Constant "c", Constant "d"]) 
                       (Function "a" [Constant "c", Constant "d"]), works, "function name match, args match")

        test (unify [] (Function "a" [Constant "c", Constant "d"]) 
                       (Function "a" [Constant "c", Constant "e"]), fails, "function args mismatch")

        test (unify [] (Function "a" [Function "b" [Constant "c"]])
                       (Function "a" [Function "b" [Constant "c"]]), works, "nested functions match")

        test (unify [] (Function "a" [Function "b" [Constant "c"]])
                       (Function "a" [Function "b" [Constant "d"]]), fails, "nested functions mismatch args")
              
        test (unify [] (Function "a" [Function "b" [Constant "c"]])
                       (Function "a" [Function "c" [Constant "c"]]), fails, "nested functions mismatch")

        test (unify [] (Variable 1) 
                       (Variable 2), contains (1, Variable 2), "uninitialized variable unify")

        -- Self unification seems a bit odd anyway, but you self unify twice then you'll get 
        -- an infinite loop if you actually record the association in ENV
        test (unify [] (Variable 1)
                       (Variable 1), freeOf (1, Variable 1), "self unification doesnt happen")

        -- Warning:  This test case might fail by infinite looping
        test (unify [(1, Variable 2),
                     (2, Variable 1)] (Variable 1) 
                                      (Variable 2), works, "double alias avoids infinite loop")
        
        test (unify [] (Variable 1) 
                       (Constant "a"), contains (1, Constant "a"), "variable unifies with constant")

        test (unify [] (Variable 1)
                       (Function "a" []), contains (1, Function "a" []), "variable unifies with 0 arity function")

        test (unify [] (Function "a" [Variable 1])
                       (Function "a" [Constant "b"]), contains (1, Constant "b"), 
                                                      "variable in function unifies with constant")

        test (unify [] (Function "a" [Constant "b", Variable 1])
                       (Function "a" [Constant "b", Constant "c"]), 
                            contains (1, Constant "c"), 
                            "variable in arity 2 function unifies with constant")

        test (unify [] (Function "a" [Variable 1])
                       (Function "a" [Variable 2]), 
                            contains (1, Variable 2),
                            "variable in function unifies with variable in function")

        test (unify [] (Function "a" [Variable 1])
                       (Function "b" [Constant "c"]), fails, "function mismatch means no unification for parameters")

        test (unify [] (Function "a" [Function "b" [Variable 1]])
                       (Function "a" [Variable 2]), 
                            contains (2, Function "b" [Variable 1]),
                            "nested function with variable unifies with matching function with variable")

        test (unify [] (Function "a" [Function "b" [Variable 1], Variable 1])
                       (Function "a" [Variable 2, Constant "c"]),
                            contains (1, Constant "c")
                            <+> contains (2, Function "b" [Constant "c"]),
                            "multiple repeat variables and indirection works")
            



t = unify [(3, Constant "a"), 
            (1, Variable 3)] (Variable 1)
                             (Variable 2)

u = unify [(3, Constant "a"), 
            (1, Variable 3)] (Variable 1)
                             (Constant "b") -- this will need to return Nothing


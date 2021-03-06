
module Main where

import Parse
import ParseLang
import ParseAst
import Control.Applicative


i = makeParseString

testLambda = test getLambdaTerm 
testAssignment = test getAssignment 
testAssignments = test getAssignments

test parser input result = let (Just t, o) = parse parser (i input) in
    case result == t of
        True -> "Pass " ++ show t 
        False -> "Fail " ++ show t


tests = 
    [ testLambda "\\ a . a" $ Abs "a" $ Var "a" 
    , testLambda "\\ a . \\ b . a" $ Abs "a" $ Abs "b" (Var "a")
    , testLambda "\\ a . a a" $ Abs "a" $ App (Var "a") (Var "a")
    , testLambda "\\ a . a a" $ Abs "a" $ App (Var "a") (Var "a")
    , testLambda "_variable123" $ Var "_variable123" 
    , testLambda "a b" $ App (Var "a") (Var "b")
    , testLambda "(\\ a . a) (\\ b . b)" $ App (Abs "a" $ Var "a" ) (Abs "b" $ Var "b") 
    , testLambda "(\\ a . a) (\\ b . b) c" $ App (App (Abs "a" $ Var "a" ) (Abs "b" $ Var "b")) (Var "c")
    , testLambda "a b c d" $ App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d")
    , testLambda "(a)(b)" $ App (Var "a") (Var "b")
    , testLambda "a(b)" $ App (Var "a") (Var "b")
    -- TODO fix this parsing case (fails now b/c the end paren is consumed by previous parse)
    , testLambda "(a)b" $ App (Var "a") (Var "b")
    , testAssignment "blah = \\ a . a;" $ Assignment "blah" (Abs "a" (Var "a"))
    , testAssignments "blah = \\ a . a;" [Assignment "blah" (Abs "a" (Var "a"))]
    , testAssignments "blah = \\ a . a; ikky = \\ b . b;" 
        [ Assignment "blah" (Abs "a" (Var "a"))
        , Assignment "ikky" (Abs "b" (Var "b"))
        ]
    ]

main = mapM_ putStrLn tests


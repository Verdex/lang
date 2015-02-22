
module Main where

import System.Environment
import System.IO

import Control.Applicative

import LangAst
import Tokenizer
import LangParser
import TypeCheck

(|>) a f = f a
infixl 0 |>

-- TODO need to prefill environment from DefineSig
-- TODO probably need to move this to typecheck
computeTypes :: (Env -> Expr -> Maybe Type) -> Program -> [(String,Maybe Type)]
computeTypes _ ParseError = []
computeTypes to (Program tls) = map onlyValue tls 
                                -- Personally I think this line looks cool, but map mToL |> concat should go first
                                -- so that I don't need the <$> (although having <$> along for the ride does make
                                -- me feel safer)
                              |> map ((\ (ValueDef n e) -> (n, to [] e)) <$>)
                              |> map mToL 
                              |> concat 

    where onlyValue (DefineSig _) = Nothing 
          onlyValue (TypeDefine _) = Nothing 
          onlyValue (Define value) = Just value 

          mToL Nothing = []
          mToL (Just a) = [a]

main = 
    do
        arg : _ <- getArgs
        contents <- readFile arg
        toks <- pure $ tokenize contents
        --putStrLn $ show toks
        prog <- pure $ parse toks
        --putStrLn $ show prog 
        types <- pure $ computeTypes typeOf prog
        putStrLn $ show types

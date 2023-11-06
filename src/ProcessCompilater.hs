{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- ProcessCompilater
-}

module ProcessCompilater (
    processCompilater
) where

import Lib
import Tokenizer
import Expression
import AST
import BCgeneration

processCompilater :: Bool -> String -> IO ()
processCompilater bool filename = case readFileContent filename of
    Left err -> printErrorAndExit err
    Right content -> case (parseTokens (tokenize content) []) of
            Left err -> printErrorAndExit err
            Right tokens' -> processSexpr bool filename tokens'


processSexpr :: Bool -> String -> [SExpr] -> IO ()
processSexpr toShow filename sexpr = 
    case parseAll sexpr of
        Left err -> printErrorAndExit err
        Right ast -> case generateBC ast of
                Left err -> printErrorAndExit err
                Right bc -> writeFileContent (filename ++ ".go") (show bc) 
                >> if toShow then putStrLn (show ast) else return ()

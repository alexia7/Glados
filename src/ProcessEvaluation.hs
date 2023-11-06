{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- ProcessEvaluation
-}

module ProcessEvaluation (
    processEvaluation
) where

import Bytecode
import Lib (printErrorAndExit, readFileContent)
import BytecodeParsing (parseBCfile)
import VirtualMachine (exec)



processEvaluation :: String -> IO ()
processEvaluation filename = case readFileContent filename of
    Left err -> printErrorAndExit err
    Right content -> case parseBCfile content of
        Left err -> printErrorAndExit err
        Right bytecode -> case exec ([], bytecode, [], [], []) of
            Left err -> printErrorAndExit err
            Right (ValueItem result, env) -> showResult result env
            Right (BuiltinsItem _, _) -> print "Error: Result are Builtin"


showResult :: Value -> Env -> IO ()
showResult (StringValue str) env = case getIfSymbol str env of
    StringValue str' -> print str'
    IntValue n -> print n
    FloatValue n -> print n
    BoolValue n -> print n
    _ -> print str
showResult (IntValue n) _ = print n
showResult (FloatValue n) _ = print n
showResult (BoolValue n) _ = print n
showResult value _ = print value


getIfSymbol :: String -> Env -> Value
getIfSymbol str [] = StringValue str
getIfSymbol str ((symbol, value):env) = if str == symbol then value else getIfSymbol str env


{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- BytecodeParsing
-}

module BytecodeParsing (
    parseBCfile,
) where

import Lib (getOnlyValue)
import Bytecode

parseBCfile :: String -> Either String [Instruction]
parseBCfile [] = Left "Error: Empty file"
parseBCfile "[]" = Left "Error: No instructions in file"
parseBCfile ('[':content) = case parseInstructions content of
    Right (instructions, "") -> Right instructions
    Right (_, remaining) -> Left $ "Error: Invalid file format, remaining characters: " ++ remaining
    Left errMsg -> Left errMsg
parseBCfile _ = Left "Error: Invalid file format, missing ["


parseInstructions :: String -> Either String ([Instruction], String)
parseInstructions (']':rest) = Right ([], rest)
parseInstructions (',':rest) = parseInstructions rest
parseInstructions input = case parseInstruction input of
        Right (instruction, remaining) -> case parseInstructions remaining of
            Right (instructions, rest) -> Right (instruction : instructions, rest)
            Left errMsg -> Left errMsg
        Left errMsg -> Left errMsg


parseInstruction :: String -> Either String (Instruction, String)
parseInstruction ('P':'u':'s':'h':' ':rest) = case getOnlyValue rest of
    Right (value, remaining) -> case parseValue value of
        Right (arg) -> Right (Push arg, remaining)
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg
parseInstruction ('A':'d':'d':'B':'u':'i':'l':'t':'i':'n':'s':' ':rest) = case parseBuiltin rest of
    Right (arg, remaining) -> Right (AddBuiltins arg, remaining)
    Left errMsg -> Left errMsg
parseInstruction ('S':'e':'t':'A':'r':'g':' ':'"':rest) = case parseString rest of
    Right (arg, remaining) -> case getOnlyValue remaining of
        Right (value, rest') -> case parseValue value of
            Right (val) -> Right (SetArg arg val, rest')
            Left errMsg -> Left errMsg
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg
parseInstruction ('S':'e':'t':' ':'"':rest) = case parseString rest of
    Right (arg, remaining) -> case getOnlyValue remaining of
        Right (value, rest') -> case parseValue value of
            Right (val) -> Right (Set arg val, rest')
            Left errMsg -> Left errMsg
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg
parseInstruction ('G':'e':'t':' ':'"':rest) = case parseString rest of
    Right (arg, remaining) -> Right (Get arg, remaining)
    Left errMsg -> Left errMsg
parseInstruction ('C':'a':'l':'l':rest) = Right (Call, rest)
parseInstruction ('P':'u':'s':'h':'A':'r':'g':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (PushArg arg, remaining)
    Left errMsg -> Left errMsg
parseInstruction ('J':'u':'m':'p':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Jump arg, remaining)
    Left errMsg -> Left errMsg
parseInstruction ('J':'u':'m':'p':'I':'f':'F':'a':'l':'s':'e':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (JumpIfFalse arg, remaining)
    Left errMsg -> Left errMsg
parseInstruction ('R':'e':'t':'u':'r':'n':rest) = Right (Return, rest)
parseInstruction instruction = Left ("Error: Invalid instruction: "++ instruction)


parseBuiltin :: String -> Either String (Builtins, String)
parseBuiltin ('(':'A':'d':'d':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Add arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('A':'d':'d':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Add arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('(':'S':'u':'b':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Sub arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('S':'u':'b':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Sub arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('(':'M':'u':'l':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Mul arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('M':'u':'l':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Mul arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('(':'D':'i':'v':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Div arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('D':'i':'v':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Div arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('(':'M':'o':'d':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Mod arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('M':'o':'d':' ':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (Mod arg, (tail remaining))
    Left errMsg -> Left errMsg
parseBuiltin ('E':'q':rest) = Right (Eq, rest)
parseBuiltin ('L':'e':'s':'s':rest) = Right (Less, rest)
parseBuiltin ('G':'r':'e':'a':'t':'e':'r':rest) = Right (Greater, rest)
parseBuiltin ('C':'o':'n':'c':'a':'t':rest) = Right (Concat, rest)
parseBuiltin ('S':'a':'m':'e':rest) = Right (Same, rest)
parseBuiltin ('S':'i':'z':'e':rest) = Right (Size, rest)
parseBuiltin ('P':'r':'i':'n':'t':rest) = Right (Print, rest)
parseBuiltin ('F':'p':'r':'i':'n':'t':rest) = Right (FPrint, rest)
parseBuiltin ('W':'r':'i':'t':'e':rest) = Right (Write, rest)
parseBuiltin ('R':'e':'a':'d':rest) = Right (Read, rest)
parseBuiltin ('N':'o':'t':rest) = Right (Not, rest)
parseBuiltin built = Left ("Error: Invalid builtin: "++ built)


parseValue :: [String] -> Either String Value
parseValue [] = Left "Error: Invalid Value"
parseValue ("IntValue":x:[]) = case parseInt x of
    Right (number, []) -> Right (IntValue number)
    Right (_, arg) -> Left ("Error: Invalid argument: " ++ show arg)
    Left errMsg -> Left errMsg
parseValue ("FloatValue":x:[]) = case parseFloat x of
    Right (number, []) -> Right (FloatValue number)
    Right (_, arg) -> Left ("Error: Invalid argument: " ++ show arg)
    Left errMsg -> Left errMsg
parseValue ("BoolValue":"True":[]) = Right (BoolValue True)
parseValue ("BoolValue":"False":[]) = Right (BoolValue False)
parseValue ("StringValue":x) = case parseString (tail (unwords x)) of
    Right (arg, []) -> Right (StringValue arg)
    Right (_, remaining) -> Left ("Error: Invalid argument: " ++ show remaining)
    Left errMsg -> Left errMsg
parseValue ("FunctionValue":rest) = parseFunction rest
parseValue value = Left ("Error: Invalid value: "++ show value)


parseInt :: String -> Either String (Int, String)
parseInt (' ':rest) = parseInt rest
parseInt ('-':rest) = case parseInt rest of
    Right (arg, remaining) -> Right (-arg, remaining)
    Left errMsg -> Left errMsg
parseInt number = case reads number :: [(Int, String)] of
    [(arg, remaining)] -> Right (arg, remaining)
    _ -> Left ("Error: Invalid argument: " ++ number)


parseFloat :: String -> Either String (Double, String)
parseFloat (' ':rest) = parseFloat rest
parseFloat ('-':rest) = case parseFloat rest of
    Right (arg, remaining) -> Right (-arg, remaining)
    Left errMsg -> Left errMsg
parseFloat number = case reads number :: [(Double, String)] of
    [(arg, remaining)] -> Right (arg, remaining)
    _ -> Left ("Error: Invalid argument: " ++ number)


parseString :: String -> Either String (String, String)
parseString [] = Right ("", [])
parseString (',':rest) = Right ("", (',':rest))
parseString (']':rest) = Right ("", (']':rest))
parseString ('"':rest) = Right ("", rest)
parseString (x:xs) = case parseString xs of
    Right (arg, remaining) -> Right ((x:arg), remaining)
    Left errMsg -> Left errMsg


parseFunction :: [String] -> Either String Value
parseFunction [] = Left "Error: Invalid function"
parseFunction (x:xs) = case parseArgs x of
    Right (args, _) -> case parseBCfile (unwords xs) of
        Right instructions -> Right (FunctionValue args instructions)
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg


parseArgs :: String -> Either String ([String], String)
parseArgs "" = Left "Error: Invalid args"
parseArgs ('[':rest) = parseArgs rest
parseArgs (',':rest) = parseArgs rest
parseArgs (']':rest) = Right ([], rest)
parseArgs ('"':arg) = case parseString arg of
    Right (arg', rest) -> case parseArgs rest of
        Right (args, rest') -> Right (arg':args, rest')
        Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg
parseArgs arg = Left ("Error: Invalid args: " ++ show arg)

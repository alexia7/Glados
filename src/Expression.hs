{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Expression
-}

module Expression (
    SExpr(..),
    parseTokens
) where

import Tokenizer

data SExpr = Number Int
            | Float Double
            | Boolean Bool
            | String String
            | Symbol String
            | TypeList SExpr [SExpr]
            | List [SExpr]
            deriving (Eq, Show)


parseTokens :: [Token] -> [SExpr] -> Either String [SExpr]
parseTokens [] [] = Left "Empty expression"
parseTokens [] expr = Right (reverse expr)
parseTokens (TokenOpenParen:xs) expr = case parseListToken xs [] of
    Left err -> Left err
    Right (sexpr, tokens) -> parseTokens tokens (sexpr:expr)
parseTokens (TokenOpenBracket:xs) expr = case parseType xs of
    Left err -> Left err
    Right (sexpr, tokens) -> parseTokens tokens (sexpr:expr)
parseTokens (x:xs) expr = case tokenToSExpr x of
    Left err -> Left err
    Right sexpr -> parseTokens xs (sexpr:expr)


tokenToSExpr :: Token -> Either String SExpr
tokenToSExpr (TokenFloat f) = Right (Float f)
tokenToSExpr (TokenNumber n) = Right (Number n)
tokenToSExpr (TokenSymbol s) = Right (Symbol s)
tokenToSExpr (TokenError e) = Left e
tokenToSExpr token = Left ("Invalid token " ++ (show token))


    -- HANDLE LIST --


parseListToken :: [Token] -> [SExpr] -> Either String (SExpr, [Token])
parseListToken [] _ = Left "Missing Closing Parenthesis"
parseListToken (TokenCloseParen:tokens) expr = Right ((List (reverse expr)), tokens)
parseListToken (TokenOpenParen:xs) expr = case parseListToken xs [] of
    Left err -> Left err
    Right (sexpr, tokens) -> parseListToken tokens (sexpr:expr)
parseListToken (TokenOpenBracket:xs) expr = case parseType xs of
    Left err -> Left err
    Right (sexpr, tokens) -> parseListToken tokens (sexpr:expr)
parseListToken (x:xs) expr = case tokenToSExpr x of
    Left err -> Left err
    Right sexpr -> parseListToken xs (sexpr:expr)


    -- HANDLE TYPE --


parseType :: [Token] -> Either String (SExpr, [Token])
parseType [] = Left "Empty expression"
parseType ((TokenSymbol value):TokenOpenParen:xs) = parseTypedList (Symbol value) xs []
parseType ((TokenSymbol "Int"):value:TokenCloseBracket:xs) = case isNumber value of
    Left err -> Left err
    Right sexpr -> Right (sexpr, xs)
parseType ((TokenSymbol "Float"):value:TokenCloseBracket:xs) = case isFloat value of
    Left err -> Left err
    Right sexpr -> Right (sexpr, xs)
parseType ((TokenSymbol "Bool"):value:TokenCloseBracket:xs) = case isBoolean value of
    Left err -> Left err
    Right sexpr -> Right (sexpr, xs)
parseType ((TokenSymbol "String"):value:TokenCloseBracket:xs) = case isString value of
    Left err -> Left err
    Right sexpr -> Right (sexpr, xs)
parseType (x:_) = Left ("Invalid type: " ++ show x)


isNumber :: Token -> Either String SExpr
isNumber (TokenNumber n) = Right (Number n)
isNumber token = Left ("Invalid number: " ++ show token)


isFloat :: Token -> Either String SExpr
isFloat (TokenFloat f) = Right (Float f)
isFloat token = Left ("Invalid float: " ++ show token)


isBoolean :: Token -> Either String SExpr
isBoolean (TokenSymbol "#t") = Right (Boolean True)
isBoolean (TokenSymbol "#f") = Right (Boolean False)
isBoolean token = Left ("Invalid boolean: " ++ show token)


isString :: Token -> Either String SExpr
isString (TokenSymbol s) = case checkQuotes s of
    Just s' -> Right (String s')
    Nothing -> Left ("Invalid string: " ++ show s)
isString token = Left ("Invalid string: " ++ show token)


checkQuotes :: String -> Maybe String
checkQuotes input =
  if not (null input) && head input == '"' && last input == '"'
    then Just (init (tail input))
    else Nothing


    -- HANDLE TYPED LIST--


parseTypedList :: SExpr -> [Token] -> [SExpr] -> Either String (SExpr, [Token])
parseTypedList _ [] _ = Left "Missing Closing Bracket"
parseTypedList t (TokenCloseParen:TokenCloseBracket:xs) expr = Right ((TypeList t (reverse expr)), xs)
parseTypedList (Symbol "Int") tokens expr = case isNumberList tokens of
    Left err -> Left err
    Right (sexpr, tokens') -> parseTypedList (Symbol "Int") tokens' (sexpr:expr)
parseTypedList (Symbol "Float") tokens expr = case isFloatList tokens of
    Left err -> Left err
    Right (sexpr, tokens') -> parseTypedList (Symbol "Float") tokens' (sexpr:expr)
parseTypedList (Symbol "Bool") tokens expr = case isBooleanList tokens of
    Left err -> Left err
    Right (sexpr, tokens') -> parseTypedList (Symbol "Bool") tokens' (sexpr:expr)
parseTypedList (Symbol "String") tokens expr = case isStringList tokens of
    Left err -> Left err
    Right (sexpr, tokens') -> parseTypedList (Symbol "String") tokens' (sexpr:expr)
parseTypedList _ _ _ = Left "Unexpected Error on parseTypedList"


isNumberList :: [Token] -> Either String (SExpr, [Token])
isNumberList (TokenOpenBracket:(TokenSymbol "Int"):(TokenNumber n):TokenCloseBracket:xs) = Right ((Number n), xs)
isNumberList _ = Left ("Invalid number in list")


isFloatList :: [Token] -> Either String (SExpr, [Token])
isFloatList (TokenOpenBracket:(TokenSymbol "Float"):(TokenFloat f):TokenCloseBracket:xs) = Right ((Float f), xs)
isFloatList _ = Left ("Invalid float in list")


isBooleanList :: [Token] -> Either String (SExpr, [Token])
isBooleanList (TokenOpenBracket:(TokenSymbol "Bool"):(TokenSymbol "#t"):TokenCloseBracket:xs) = Right ((Boolean True), xs)
isBooleanList (TokenOpenBracket:(TokenSymbol "Bool"):(TokenSymbol "#f"):TokenCloseBracket:xs) = Right ((Boolean False), xs)
isBooleanList _ = Left ("Invalid boolean in list")


isStringList :: [Token] -> Either String (SExpr, [Token])
isStringList (TokenOpenBracket:(TokenSymbol "String"):(TokenSymbol s):TokenCloseBracket:xs) = case checkQuotes s of
    Just s' -> Right ((String s'), xs)
    Nothing -> Left ("Invalid string in list")
isStringList _ = Left ("Invalid String in list")

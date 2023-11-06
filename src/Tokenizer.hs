{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Tokenizer
-}

module Tokenizer (
    Token(..),
    tokenize
) where

import Data.Char

data Token = TokenSymbol String
           | TokenNumber Int
           | TokenFloat Double
           | TokenOpenParen
           | TokenCloseParen
           | TokenOpenBracket
           | TokenCloseBracket
           | TokenError String
           deriving (Eq, Show)


tokenize :: String -> [Token]
tokenize [] = []
tokenize ('-':c:cs)
    | isDigit c = tokenizeNumber (getOnlyNumber ('-':c:cs) "")
    | otherwise = tokenizeSymbol ('-':c:cs)
tokenize ('"':cs) = tokenizeString cs
tokenize ('(':cs) = TokenOpenParen : tokenize cs
tokenize (')':cs) = TokenCloseParen : tokenize cs
tokenize ('[':cs) = TokenOpenBracket : tokenize cs
tokenize (']':cs) = TokenCloseBracket : tokenize cs
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = tokenizeNumber (getOnlyNumber (c:cs) "")
    | otherwise = tokenizeSymbol (c:cs)


getOnlyNumber :: String -> String -> (String, String)
getOnlyNumber [] tmp = (reverse tmp, [])
getOnlyNumber ('.':xs) tmp = getOnlyNumber xs ('.':tmp)
getOnlyNumber ('-':xs) tmp = getOnlyNumber xs ('-':tmp)
getOnlyNumber (x:xs) tmp
    | isDigit x = getOnlyNumber xs (x:tmp)
    | otherwise = (reverse tmp, x:xs)


tokenizeNumber :: (String, String) -> [Token]
tokenizeNumber (str, rest) =
  case strToNum str of
    Just num -> if '.' `elem` str
                then TokenFloat num : tokenize rest
                else TokenNumber (round num) : tokenize rest
    Nothing  -> TokenError "Invalid number format": tokenize rest


strToNum :: String -> Maybe Double
strToNum str
  | head str == '-' = case reads str of
                       [(num, "")] -> Just num
                       _           -> Nothing
  | otherwise = case reads str of
                 [(num, "")] -> Just num
                 _           -> Nothing


tokenizeSymbol :: String -> [Token]
tokenizeSymbol cs =
    let (sym, rest) = span (\c -> isAlphaNum c || c `elem` "+-*/%:?#<>") cs
    in TokenSymbol sym : tokenize rest


tokenizeString :: String -> [Token]
tokenizeString cs = tokenizeString' cs "\""
  where
    tokenizeString' ('"':rest) acc = TokenSymbol (reverse acc++"\"") : tokenize rest
    tokenizeString' ('\\':'"':rest) acc = tokenizeString' rest ('"':acc)
    tokenizeString' (c:rest) acc = tokenizeString' rest (c:acc)
    tokenizeString' [] _ = [TokenError "String not terminated with a closing quote"]

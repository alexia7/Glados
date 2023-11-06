--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Main
--

module Main (main) where

import ArgHandling
import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    case checkFlags (processArgument args) of
        Left err -> printErrorAndExit err
        Right flags -> flagRedirection flags

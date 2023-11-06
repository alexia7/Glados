{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- ProcessAll
-}

module ProcessAll (
    processAll
) where

import Lib
import ProcessCompilater
import ProcessEvaluation


processAll :: String -> IO ()
processAll filename = processCompilater False filename >>
    processEvaluation (changeExtension filename)

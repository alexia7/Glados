{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- ArgHandling
-}

module ArgHandling (
    Flag(..),
    processArgument,
    checkFlags,
    flagRedirection
) where

import Lib(validateExtension, printHelper, printErrorAndExit)
import ProcessCompilater(processCompilater)
import ProcessEvaluation(processEvaluation)
import ProcessAll(processAll)

data Flag = Compiler
          | Ast
          | Help
          | Evaluate
          | Filename String
          | All
          deriving (Eq, Show)


processArgument :: [String] -> [Flag]
processArgument [] = []
processArgument ("-help":xs) = (Help:(processArgument xs))
processArgument ("-ast":xs) = (Ast:(processArgument xs))
processArgument ("-evaluate":xs) = (Evaluate:(processArgument xs))
processArgument ("-compiler":xs) = (Compiler:(processArgument xs))
processArgument (name:xs) = ((Filename name):(processArgument xs))


checkFlags :: [Flag] -> Either String [Flag]
checkFlags [] = Left "Missing arguments."
checkFlags [Help] = Right [Help]
checkFlags [Filename name] = case validateExtension "scm" name of
    True -> Right [All, Filename name]
    False -> Left "Invalid file extension."
checkFlags [Compiler, Ast, Filename name] = case validateExtension "scm" name of
    True -> Right [Compiler, Ast, Filename name]
    False -> Left "Invalid file extension."
checkFlags [Ast, Compiler, Filename name] = case validateExtension "scm" name of
    True -> Right [Compiler, Ast, Filename name]
    False -> Left "Invalid file extension."
checkFlags [Compiler, Filename name] = case validateExtension "scm" name of
    True -> Right [Compiler, Filename name]
    False -> Left "Invalid file extension."
checkFlags [Evaluate, Filename name] = case validateExtension "go" name of
    True -> Right [Evaluate, Filename name]
    False -> Left "Invalid file extension."
checkFlags _ = Left "Invalid arguments."


flagRedirection :: [Flag] -> IO()
flagRedirection [Help] = printHelper
flagRedirection [All, Filename name] = processAll name
flagRedirection [Compiler, Ast, Filename name] = processCompilater True name
flagRedirection [Compiler, Filename name] = processCompilater False name
flagRedirection [Evaluate, Filename name] = processEvaluation name
flagRedirection _ = printErrorAndExit "A problem occured."


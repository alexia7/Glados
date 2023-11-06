{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Lib
-}

module Lib (
    printHelper,
    printErrorAndExit,
    isOperator,
    eitherToMaybe,
    validateExtension,
    readFileContent,
    writeFileContent,
    changeExtension,
    getBetweenParenthesis,
    getOnlyValue,
    getFileExtension,
) where

import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Exit
import Control.Exception

printHelper :: IO()
printHelper = putStrLn "USAGE:\n\t./glados file.scm                    Compile the file.scm into file.go then evaluate it.\n\t./glados -compiler [-ast] file.scm   Compile the file.scm into file.go, ast is optional and will print the ast.\n\t./glados -evaluate file.go           Evaluate the file.go.\n\t./glados -help                       Print this helper. \n"


printErrorAndExit :: String -> IO a
printErrorAndExit errorMsg = hPutStrLn stderr errorMsg 
    >> exitWith (ExitFailure 84)


isOperator :: String -> Bool
isOperator s = s `elem` ["+", "-", "*", "div", "mod", "eq?", "<", ">", "++", "concat", "same", "==", "head", "tail", "at", "size", "print", "fprint", ">>", "<<", "not"]


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing


validateExtension :: String -> String -> Bool
validateExtension ext filename
    | getFileExtension filename == ext = True
    | otherwise = False


getFileExtension :: String -> String
getFileExtension filename = reverse (takeWhile (/= '.') (reverse filename))


readFileContent :: FilePath -> Either String String
readFileContent filePath = unsafePerformIO $ catch (Right <$> readFile filePath) handleException
  where
    handleException :: IOError -> IO (Either String String)
    handleException e
        | isDoesNotExistError e = return $ Left "Le fichier n'existe pas."
        | otherwise = return $ Left $ "Erreur lors de la lecture du fichier : " ++ show e


writeFileContent :: String -> String -> IO ()
writeFileContent filePath content = writeFile (changeExtension filePath) content


changeExtension :: String -> String
changeExtension input =
  let parts = splitOn '.' input
  in case parts of
    [] -> input
    (name:_) -> name ++ ".go"


splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delimiter str =
  let (before, after) = break (== delimiter) str
  in
    before :
    case after of
      [] -> []
      _:xs -> splitOn delimiter xs


getBetweenParenthesis :: String -> Int -> Either String (String, String)
getBetweenParenthesis "" _ = Left "Error: Missing parenthesis"
getBetweenParenthesis ('(':rest) nb = getBetweenParenthesis rest (nb + 1)
getBetweenParenthesis (')':rest) nb = case nb of
    1 -> Right ("", rest)
    _ -> case getBetweenParenthesis rest (nb - 1) of
        Right (arg, remaining) -> Right (')':arg, remaining)
        Left errMsg -> Left errMsg
getBetweenParenthesis (x:xs) nb = case getBetweenParenthesis xs nb of
    Right (arg, remaining) -> Right (x:arg, remaining)
    Left errMsg -> Left errMsg


getOnlyValue :: String -> Either String ([String], String)
getOnlyValue "" = Left "Error: Empty value"
getOnlyValue (' ':str) = getOnlyValue str
getOnlyValue str = case getBetweenParenthesis str 0 of
    Right (arg, remaining) -> Right ((splitOn ' ' arg), remaining)
    Left errMsg -> Left errMsg

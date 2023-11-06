{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- AST
-}

module AST (
    Ast(..),
    parseAll,
    parseToAST,
    parseOperator,
    parseIf,
    parseArray,
    parseDefine,
    parseLambda,
    parseWhile,
    parseFor,
    parseArgList
) where

import Lib
import Expression

data Ast = AstNumber Int                      -- Int
         | AstFloat Double                    -- Double
         | AstBool Bool                       -- Bool
         | AstString String                   -- String
         | AstTypedList Ast [Ast]             -- TypeList type list
         | AstSymbol String                   -- nom de variable
         | Array [Ast]                        -- Array (liste)
         | Define String Ast                  -- Define varName valeur type
         | UnOp String Ast                    -- UnOp operateur valeur
         | BinOp String Ast Ast               -- BinOp operateur valeur1 valeur2
         | InfOp String Ast                   -- InfOp operateur listeValeur
         | Argument Ast String                -- Argument symbole type
         | Function String [Ast] Ast          -- Function nom listeArgument execution
         | Lambda [Ast] Ast                   -- Lambda listeArgument execution
         | If Ast Ast Ast                     -- If condition executionVrai executionFaux
         | While Ast [Ast]                    -- While condition execution
         | For Ast Ast Ast [Ast]              -- For init condition increment execution
         | Switch Ast [(Ast, [Ast])] [Ast]    -- Switch condition [(valeur, execution)] [executionDefaut]
         | ASTReturn                          -- Return
         deriving (Show, Eq, Ord)


parseAll :: [SExpr] -> Either String Ast
parseAll exprs = case traverse parseToAST exprs of
    Right asts -> Right (Array asts)
    Left err -> Left err


parseToAST :: SExpr -> Either String Ast
parseToAST (Number n) = Right (AstNumber n)
parseToAST (Float f) = Right (AstFloat f)
parseToAST (Boolean b) = Right (AstBool b)
parseToAST (Symbol "#t") = Right (AstBool True)
parseToAST (Symbol "#f") = Right (AstBool False)
parseToAST (String s) = Right (AstString s)
parseToAST (Symbol s) = Right (AstSymbol s)
parseToAST (TypeList x xs) = case parseToAST x of
    Right x' -> case parseToAST (List xs) of
        Right (Array xs') -> Right (AstTypedList x' xs')
        Right ast -> Right (AstTypedList x' [ast])
        Left err -> Left err
    Left err -> Left err
parseToAST (List l) = parseArray l


parseArray :: [SExpr] -> Either String Ast
parseArray [] = Right (Array [])
parseArray (Symbol s : expr) = case s of
    "if" -> parseIf expr
    "define" -> parseDefine expr
    "lambda" -> parseLambda expr
    "while" -> parseWhile expr
    "for" -> parseFor expr
    _ -> case isOperator s of
        True -> parseOperator s expr
        False -> createArray (Symbol s : expr)
parseArray expr = createArray expr


createArray :: [SExpr] -> Either String Ast
createArray [] = Right (Array [])
createArray (x:xs) = case parseToAST x of
    Right x' -> case createArray xs of
        Right (Array xs') -> Right (Array (x':xs'))
        Right ast -> Right (Array [x', ast])
        Left err -> Left err
    Left err -> Left err


parseIf :: [SExpr] -> Either String Ast
parseIf expr = case expr of
    [condExpr, ifTrueExpr, ifFalseExpr] -> case parseToAST condExpr of
        Right cond -> case parseToAST ifTrueExpr of
            Right ifTrue -> case parseToAST ifFalseExpr of
                Right ifFalse -> Right (If cond ifTrue ifFalse)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid if"


parseDefine :: [SExpr] -> Either String Ast
parseDefine expr = case expr of
    [Symbol s, List argsExpr, bodyExpr] -> case parseArgList argsExpr of
        Right args -> case parseToAST bodyExpr of
            Right body -> Right (Define s (Function s args body))
            Left err -> Left err
        Left err -> Left err
    [Symbol s, expr'] -> case parseToAST expr' of
        Right expr'' -> Right (Define s expr'')
        Left err -> Left err
    [List (Symbol s : args), expr'] -> case parseArgList args of
        Right args' -> case parseToAST expr' of
            Right expr'' -> Right (Define s (Function s args' expr''))
            Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid define"


parseLambda :: [SExpr] -> Either String Ast
parseLambda expr = case expr of
    [List argsExpr, bodyExpr] -> case parseArgList argsExpr of
        Right args -> case parseToAST bodyExpr of
            Right body -> Right (Lambda args body)
            Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid lambda"


parseArgList :: [SExpr] -> Either String [Ast]
parseArgList [] = Right []
parseArgList (List argsExpr : rest) = case traverse parseToAST argsExpr of
    Right args -> do
        restArgs <- parseArgList rest
        Right (args ++ restArgs)
    Left err -> Left err
parseArgList (Symbol argExpr : rest) =
    case break (== ':') argExpr of
        (arg, ':' : argType) -> do
            restArgs <- parseArgList rest
            Right (Argument (AstSymbol arg) argType : restArgs)
        _ -> Left "Invalid argument type"
parseArgList _ = Left "Invalid argument list"


parseWhile :: [SExpr] -> Either String Ast
parseWhile expr = case expr of
    [condExpr, bodyExpr1] -> case parseToAST condExpr of
        Right cond -> case parseToAST bodyExpr1 of
            Right body1 -> Right (While cond [body1])
            Left err -> Left err
        Left err -> Left err
    [condExpr, bodyExpr1, bodyExpr2] -> case parseToAST condExpr of
        Right cond -> case parseToAST bodyExpr1 of
            Right body1 -> case parseToAST bodyExpr2 of
                Right body2 -> Right (While cond [body1, body2])
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid while"


parseFor :: [SExpr] -> Either String Ast
parseFor expr = case expr of
    [initExpr, condExpr, incExpr, List bodyExprs] -> case traverse parseToAST [initExpr, condExpr, incExpr] of
        Right [init', cond, inc] -> case traverse parseToAST bodyExprs of
            Right body -> Right (For init' cond inc body)
            Left err -> Left err
        _ -> Left "Invalid body expressions"
    _ -> Left "Invalid for"


parseOperator :: String -> [SExpr] -> Either String Ast
parseOperator _ [] = Left "Invalid operator: empty list"
parseOperator s (x:[]) = case parseToAST x of
    Right x' -> Right (UnOp s x')
    Left err -> Left err
parseOperator s (x':y':[]) = case parseToAST x' of
    Right x'' -> case parseToAST y' of
        Right y'' -> Right (BinOp s x'' y'')
        Left err -> Left err
    Left err -> Left err
parseOperator s expr' = case createArray expr' of
    Right expr'' -> Right (InfOp s expr'')
    Left err -> Left err

{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- BCgeneration
-}

module BCgeneration (
    generateBC,
) where

import Bytecode
import AST


generateBC :: Ast -> Either String [Instruction]
generateBC (AstNumber n) = Right [Push (IntValue n)]
generateBC (AstFloat f) = Right [Push (FloatValue f)]
generateBC (AstBool b) = Right [Push (BoolValue b)]
generateBC (AstString s) = Right [Push (StringValue s)]
generateBC (AstTypedList t (x:xs)) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateBC (AstTypedList t xs) of
        Left err -> Left err
        Right xs' -> Right (x' ++ xs')
generateBC (AstSymbol s) = Right [Get s]
generateBC (Array (x:[])) = generateBC x
generateBC (Array (x:xs)) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateBC (Array xs) of
        Left err -> Left err
        Right xs' -> Right (x' ++ xs')
generateBC (UnOp op x) = generateOperator (UnOp op x)
generateBC (BinOp op x y) = generateOperator (BinOp op x y)
generateBC (InfOp op args) = generateOperator (InfOp op args)
generateBC (Define s x) = generateDefine (Define s x)
generateBC (If cond true false) = generateIf cond true false
generateBC (While cond body) = generateWhile cond body
generateBC (For initial cond incr body) = generateFor initial cond incr body
generateBC (Function name args body) = generateFunction name args body
generateBC (Lambda args body) = generateLambda args body
generateBC _ = Left "Error: Invalid AST, failed to build Bytecode"


generateDefine :: Ast -> Either String [Instruction]
generateDefine (Define s (AstNumber x)) = Right ([Set s (IntValue x)])
generateDefine (Define s (AstFloat x)) = Right ([Set s (FloatValue x)])
generateDefine (Define s (AstBool x)) = Right ([Set s (BoolValue x)])
generateDefine (Define s (AstString x)) = Right ([Set s (StringValue x)])
generateDefine (Define s (AstTypedList t (x:xs))) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateBC (AstTypedList t xs) of
        Left err -> Left err
        Right xs' -> Right (x' ++ xs' ++ [Set s (ListValue [])])
generateDefine (Define s (AstSymbol x)) = Right ([Set s (StringValue x)])
generateDefine (Define s (Array (x:[]))) = generateDefine (Define s x)
generateDefine (Define s (Array (x:xs))) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateBC (Array xs) of
        Left err -> Left err
        Right xs' -> Right (x' ++ xs' ++ [Set s (ListValue [])])
generateDefine (Define s (Define s' x)) = case generateDefine (Define s' x) of
    Left err -> Left err
    Right x' -> Right (x' ++ [Set s (StringValue s')])
generateDefine (Define symbol (Function name args body)) = case generateFunction name args body of
    Left err -> Left err
    Right insts -> Right ([Set symbol (FunctionValue (map (\(Argument (AstSymbol s) _) -> s) args) insts)])
generateDefine (Define symbol (Lambda args body)) = do
    lambdaInsts <- generateLambda args body
    return ([Set symbol (FunctionValue (map (\(Argument (AstSymbol s) _) -> s) args) lambdaInsts)])
generateDefine _ = Left "Error: Invalid define in AST, failed to build Bytecode"


generateFunction :: String -> [Ast] -> Ast -> Either String [Instruction]
generateFunction _ _ body = do
    body' <- generateBC body
    return (body' ++ [Return])


generateIf :: Ast -> Ast -> Ast -> Either String [Instruction]
generateIf cond true false = do
    cond' <- generateBC cond
    true' <- generateBC true
    false' <- generateBC false
    let trueLen = length true'
    let falseLen = length false'
    return (cond' ++ [JumpIfFalse (trueLen + 1)] ++ true' ++ [Jump (falseLen)] ++ false')


generateWhile :: Ast -> [Ast] -> Either String [Instruction]
generateWhile cond body = do
    cond' <- generateBC cond
    body' <- generateBC (Array body)
    let bodyLen = length body'
    return (cond' ++ [JumpIfFalse (bodyLen + 1)] ++ body' ++ [Jump (-(bodyLen + 1 + length cond'))])


generateFor :: Ast -> Ast -> Ast -> [Ast] -> Either String [Instruction]
generateFor initial cond incr body = do
    init' <- generateBC initial
    cond' <- generateBC cond
    incr' <- generateBC incr
    body' <- generateBC (Array body)
    let bodyLen = length body'
    return (init' ++ [Jump (length cond' + 1)] ++ cond' ++ [JumpIfFalse (bodyLen + length incr' + 2)] ++ body' ++ incr' ++ [Jump (-(bodyLen + length incr' + length cond' + 2 + length init'))])


generateLambda :: [Ast] -> Ast -> Either String [Instruction]
generateLambda args body = do
    body' <- generateBC body
    return ([Push (FunctionValue (map (\(Argument (AstSymbol s) _) -> s) args) body')] ++ [Return])


-- BUILTINS HANDLE --


generateOperator :: Ast -> Either String [Instruction]
generateOperator (UnOp op x) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateOperatorCall op 1 of
        Left err -> Left err
        Right op' -> Right (x' ++ [op'])
generateOperator (BinOp op x y) = case generateBC x of
    Left err -> Left err
    Right x' -> case generateBC y of
        Left err -> Left err
        Right y' -> case generateOperatorCall op 2 of
            Left err -> Left err
            Right op'' -> Right (x' ++ y' ++ [op''] ++ [Call])
generateOperator (InfOp op (Array args)) = case generateBC (Array args) of
    Left err -> Left err
    Right args' -> case generateOperatorCall op (length args) of
        Left err -> Left err
        Right op' -> Right (args' ++ [op'] ++ [Call])
generateOperator _ = Left "Error: Invalid operator in AST, failed to build Bytecode"



generateOperatorCall :: String -> Int -> Either String Instruction
generateOperatorCall "+" nb = Right (AddBuiltins (Add nb))
generateOperatorCall "-" nb = Right (AddBuiltins (Sub nb))
generateOperatorCall "*" nb = Right (AddBuiltins (Mul nb))
generateOperatorCall "div" nb = Right (AddBuiltins (Div nb))
generateOperatorCall "mod" nb = Right (AddBuiltins (Mod nb))
generateOperatorCall "eq?" _ = Right (AddBuiltins Eq)
generateOperatorCall "<" _ = Right (AddBuiltins Less)
generateOperatorCall ">" _ = Right (AddBuiltins Greater)
generateOperatorCall "++" _ = Right (AddBuiltins Concat)
generateOperatorCall "concat" _ = Right (AddBuiltins Concat)
generateOperatorCall "same" _ = Right (AddBuiltins Same)
generateOperatorCall "==" _ = Right (AddBuiltins Same)
generateOperatorCall "size" _ = Right (AddBuiltins Size)
generateOperatorCall "print" _ = Right (AddBuiltins Print)
generateOperatorCall "fprint" _ = Right (AddBuiltins FPrint)
generateOperatorCall ">>" _ = Right (AddBuiltins Write)
generateOperatorCall "<<" _ = Right (AddBuiltins Read)
generateOperatorCall "not" _ = Right (AddBuiltins Not)
generateOperatorCall op _ = Left ("Error: Invalid operator " ++ op ++ " in AST, failed to build Bytecode")

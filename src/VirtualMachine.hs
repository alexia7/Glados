{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- VirtualMachine
-}

module VirtualMachine(
    exec,
) where

import Bytecode
import Data.Fixed (mod')

exec :: VMenv -> Either String Result
exec ([], [], _, _, _) = Left "Invalid bytecode: Stack is empty"
exec (stack, [], _, env, _) = Right (head stack, env)
exec (stack, (Push value:insts), done, env, args) = exec ((ValueItem value):stack, insts, (Push value):done, env, args)
exec (stack, (AddBuiltins builtin:insts), done, env, args) = exec ((BuiltinsItem builtin):stack, insts, (AddBuiltins builtin):done, env, args)
exec (stack, (SetArg arg value:insts), done, env, args) = exec (stack, insts, (SetArg arg value):done, env, (arg, value):args)
exec (stack, (Set arg value:insts), done, env, args) = exec (stack, insts, (Set arg value):done, (arg, value):env, args)
exec (stack, (Get arg:insts), done, env, args) = case lookup arg env of
    Just (FunctionValue funcArgs funcInsts) -> case execFunction funcArgs funcInsts (stack, insts, (Get arg):done, env, args) of
        Right newEnv -> exec newEnv
        Left err -> Left err
    Just value -> exec ((ValueItem value):stack, insts, (Get arg):done, env, args)
    Nothing -> Left ("Invalid bytecode: Variable not found: "++show arg)
exec (stack, (Call:insts), done, env, args) = case execCall (stack, insts, done, env, args) of
    Right newEnv -> exec newEnv
    Left err -> Left err
exec (stack, (PushArg index:insts), done, env, args) = case execPushArg index args of
    Right value -> exec ((ValueItem value):stack, insts, (PushArg index):done, env, args)
    Left err -> Left err
exec (stack, (Jump index:insts), done, env, args) = case execJump index (stack, insts, done, env, args) of
    Right newEnv -> exec newEnv
    Left err -> Left err
exec (stack, (JumpIfFalse index:insts), done, env, args) = case head stack of
    (ValueItem (BoolValue False)) -> case execJump index (tail stack, insts, done, env, args) of
        Right newEnv -> exec newEnv
        Left err -> Left err
    (ValueItem (BoolValue True)) -> exec (tail stack, insts, (JumpIfFalse index):done, env, args)
    _ -> Left "Invalid bytecode: Invalid type for JumpIfFalse"
exec ([], (Return:_), _, _, _) = Left "Invalid bytecode: Stack is empty"
exec (stack, (Return:_), _, env, _) = Right (head stack, env)


execCall :: VMenv -> Either String VMenv
execCall ([], _, _, _, _) = Left "Invalid bytecode: No function to call"
execCall ((ValueItem (FunctionValue funcArgs funcInsts)):stack, insts, done, env, args) = case execFunction funcArgs funcInsts (stack, insts, done, env, args) of
    Right newEnv -> Right newEnv
    Left err -> Left err
execCall ((BuiltinsItem builtin):stack, insts, done, env, args) = case execBuiltins builtin stack of
    Right newStack -> Right (newStack, insts, Call:done, env, args)
    Left err -> Left err
execCall (_, _, _, _, _) = Left "Error: Invalid type for Call"


execPushArg :: Int -> ArgsEnv -> Either String Value
execPushArg _ [] = Left "Invalid bytecode: No argument to push in arg"
execPushArg index args
    | index < 0 || index >= length args = Left "Index invalid"
    | otherwise = case args !! index of
        (_, value) -> Right value


execJump :: Int -> VMenv -> Either String VMenv
execJump index (stack, insts, done, env, args)
    | index >= (length insts) + 1 = Left "Invalid bytecode: Jump index out of range"
    | index < 0 && index >= (length done) + 1 = Left "Invalid bytecode: Jump negative index"
    | index < 0 = Right (stack, (take index done)++(Jump index:insts), drop index done, env, args)
    | otherwise = Right (stack, drop index insts, (reverse (take index insts))++((Jump index):done), env, args)


    -- Execution of Fonctions


execFunction :: [String] -> [Instruction] -> VMenv -> Either String VMenv
execFunction arg instr env = case setFunctionArgs arg env of
    Right (stack, toExec, done, env', envArg) -> case exec (stack, instr, done, envArg++env', []) of
        Right (ValueItem value, env'') -> Right (ValueItem value:stack, toExec, done, env'', envArg)
        Right (BuiltinsItem builtin, env'') -> Right (BuiltinsItem builtin:stack, toExec, done, env'', [])
        Left err -> Left err
    Left err -> Left err


setFunctionArgs :: [String] -> VMenv -> Either String VMenv
setFunctionArgs [] env = Right env
setFunctionArgs (x:xs) (stack, (Push value):insts, done, env, args) = setFunctionArgs xs (stack, insts, (Push value):done, env, (x, value):args)
setFunctionArgs _ _ = Left "Invalid bytecode: Invalid function args"


    -- Execution of Builtins


execBuiltins :: Builtins -> Stack -> Either String Stack
execBuiltins (Add nb) stack = execAdd nb stack (IntValue 0)
execBuiltins (Sub nb) stack = case head (reverse (take nb stack)) of
    ValueItem value -> execSub (nb-1) ((tail (reverse (take nb stack)))++(drop nb stack)) value
    value -> Left ("Invalid bytecode: Invalid type for Sub: " ++ show value)
execBuiltins (Mul nb) stack = execMul nb stack (IntValue 1)
execBuiltins (Div _) stack = execDiv stack
execBuiltins (Mod _) stack = execMod stack
execBuiltins Eq stack = execEq stack
execBuiltins Less stack = execLess stack
execBuiltins Greater stack = execGreater stack
execBuiltins Concat stack = execConcat stack
execBuiltins Same stack = execEq stack
execBuiltins Size stack = execSize stack
execBuiltins Print _ = Left "Not implemented: fprint"
execBuiltins FPrint _ = Left "Not implemented: fprint"
execBuiltins Write _ = Left "Not implemented: write"
execBuiltins Read _ = Left "Not implemented: read"
execBuiltins Not stack = execNot stack


execAdd :: Int -> Stack -> Value -> Either String Stack
execAdd 0 stack tmp = Right ((ValueItem tmp):stack)
execAdd nb (ValueItem (IntValue value):stack) (IntValue tmp) = execAdd (nb-1) stack (IntValue (value+tmp))
execAdd nb (ValueItem (FloatValue value):stack) (FloatValue tmp) = execAdd (nb-1) stack (FloatValue (value+tmp))
execAdd nb (ValueItem (IntValue value):stack) (FloatValue tmp) = execAdd (nb-1) stack (FloatValue (fromIntegral value+tmp))
execAdd nb (ValueItem (FloatValue value):stack) (IntValue tmp) = execAdd (nb-1) stack (FloatValue (value+fromIntegral tmp))
execAdd _ value _ = Left ("Invalid bytecode: Invalid type for Add: " ++ show value)


execSub :: Int -> Stack -> Value -> Either String Stack
execSub 0 stack tmp = Right ((ValueItem tmp):stack)
execSub nb (ValueItem (IntValue value):stack) (IntValue tmp) = execSub (nb-1) stack (IntValue (tmp-value))
execSub nb (ValueItem (FloatValue value):stack) (FloatValue tmp) = execSub (nb-1) stack (FloatValue (tmp-value))
execSub nb (ValueItem (IntValue value):stack) (FloatValue tmp) = execSub (nb-1) stack (FloatValue (tmp - (fromIntegral value)))
execSub nb (ValueItem (FloatValue value):stack) (IntValue tmp) = execSub (nb-1) stack (FloatValue ((fromIntegral tmp) - value))
execSub _ value _ = Left ("Invalid bytecode: Invalid type for Sub:: " ++ show value)


execMul :: Int -> Stack -> Value -> Either String Stack
execMul 0 stack tmp = Right ((ValueItem tmp):stack)
execMul nb (ValueItem (IntValue value):stack) (IntValue tmp) = execMul (nb-1) stack (IntValue (value*tmp))
execMul nb (ValueItem (FloatValue value):stack) (FloatValue tmp) = execMul (nb-1) stack (FloatValue (value*tmp))
execMul nb (ValueItem (IntValue value):stack) (FloatValue tmp) = execMul (nb-1) stack (FloatValue (fromIntegral value*tmp))
execMul nb (ValueItem (FloatValue value):stack) (IntValue tmp) = execMul (nb-1) stack (FloatValue (value*fromIntegral tmp))
execMul _ value _ = Left ("Invalid bytecode: Invalid type for Mul: " ++ show value)


execDiv :: Stack -> Either String Stack
execDiv [] = Left "Invalid bytecode: Stack is empty"
execDiv (ValueItem (IntValue 0):_) = Left "Invalid bytecode: Division by zero"
execDiv (ValueItem (FloatValue 0):_) = Left "Invalid bytecode: Division by zero"
execDiv (ValueItem (IntValue x):ValueItem (IntValue xs):stack) = Right ((ValueItem (IntValue (xs `div` x))):stack)
execDiv (ValueItem (FloatValue x):ValueItem (FloatValue xs):stack) = Right ((ValueItem (FloatValue (xs/x))):stack)
execDiv (ValueItem (IntValue x):ValueItem (FloatValue xs):stack) = Right ((ValueItem (FloatValue (xs/(fromIntegral x)))):stack)
execDiv (ValueItem (FloatValue x):ValueItem (IntValue xs):stack) = Right ((ValueItem (FloatValue ((fromIntegral xs)/x))):stack)
execDiv value = Left ("Invalid bytecode: Invalid type for Div: " ++ show value)


execMod :: Stack -> Either String Stack
execMod [] = Left "Invalid bytecode: Stack is empty"
execMod (ValueItem (IntValue 0):_) = Left "Invalid bytecode: Modulo by zero"
execMod (ValueItem (FloatValue 0):_) = Left "Invalid bytecode: Modulo by zero"
execMod (ValueItem (IntValue x):ValueItem (IntValue xs):stack) = Right ((ValueItem (IntValue (xs `mod` x))):stack)
execMod (ValueItem (FloatValue x):ValueItem (FloatValue xs):stack) = Right ((ValueItem (FloatValue (mod' xs x))):stack)
execMod (ValueItem (IntValue x):ValueItem (FloatValue xs):stack) = Right ((ValueItem (FloatValue (mod' xs (fromIntegral x)))):stack)
execMod (ValueItem (FloatValue x):ValueItem (IntValue xs):stack) = Right ((ValueItem (FloatValue (mod' (fromIntegral xs) x))):stack)
execMod value = Left ("Invalid bytecode: Invalid type for Mod: " ++ show value)


execEq :: Stack -> Either String Stack
execEq [] = Left "Invalid bytecode: Stack is empty"
execEq (ValueItem val1:ValueItem val2:stack) = Right ((ValueItem (BoolValue (val1 == val2))):stack)
execEq value = Left ("Invalid bytecode: Invalid type for Eq: " ++ show value)


execLess :: Stack -> Either String Stack
execLess [] = Left "Invalid bytecode: Stack is empty"
execLess (ValueItem val1:ValueItem val2:stack) = Right ((ValueItem (BoolValue (val1 > val2))):stack)
execLess value = Left ("Invalid bytecode: Invalid type for Less: " ++ show value)


execGreater :: Stack -> Either String Stack
execGreater [] = Left "Invalid bytecode: Stack is empty"
execGreater (ValueItem val1:ValueItem val2:stack) = Right ((ValueItem (BoolValue (val1 < val2))):stack)
execGreater value = Left ("Invalid bytecode: Invalid type for Greater: " ++ show value)


execConcat :: Stack -> Either String Stack
execConcat [] = Left "Invalid bytecode: Stack is empty"
execConcat (ValueItem (StringValue val1):ValueItem (StringValue val2):stack) = Right ((ValueItem (StringValue (val2++val1))):stack)
execConcat value = Left ("Invalid bytecode: Invalid type for Concat: " ++ show value)


execSize :: Stack -> Either String Stack
execSize [] = Left "Invalid bytecode: Stack is empty"
execSize (ValueItem (StringValue val):stack) = Right ((ValueItem (IntValue (length val))):stack)
execSize (ValueItem (ListValue val):stack) = Right ((ValueItem (IntValue (length val))):stack)
execSize value = Left ("Invalid bytecode: Invalid type for Size: " ++ show value)


execNot :: Stack -> Either String Stack
execNot [] = Left "Invalid bytecode: Stack is empty"
execNot (ValueItem (BoolValue val):stack) = Right ((ValueItem (BoolValue (not val))):stack)
execNot value = Left ("Invalid bytecode: Invalid type for Not: " ++ show value)

{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- ByteCode
-}

module Bytecode(
    Value(..),
    Builtins(..),
    Instruction(..),
    Env,
    Result,
    VMenv,
    StackItem(..),
    ArgsEnv,
    Stack,
) where

data Value = IntValue Int
           | FloatValue Double
           | BoolValue Bool
           | StringValue String
           | ListValue [Value]
           | FunctionValue [String] [Instruction]
           deriving (Show, Eq, Ord)

data Builtins = Add Int
              | Sub Int
              | Mul Int
              | Div Int
              | Mod Int
              | Eq
              | Less
              | Greater
              | Concat
              | Same
              | Size
              | Print
              | FPrint
              | Write
              | Read
              | Not
              deriving (Show, Eq, Ord)

data Instruction = Push Value
                 | AddBuiltins Builtins
                 | SetArg String Value
                 | Set String Value
                 | Get String
                 | Call
                 | PushArg Int
                 | Jump Int
                 | JumpIfFalse Int
                 | Return
                 deriving (Show, Eq, Ord)

data StackItem = ValueItem Value | BuiltinsItem Builtins
    deriving Show

type Stack = [StackItem]
type Insts = [Instruction]
type DoneInsts = [Instruction]
type Env = [(String, Value)]
type ArgsEnv = [(String, Value)]

type Result = (StackItem, Env)

type VMenv = (Stack, Insts, DoneInsts, Env, ArgsEnv)

module Helpers (
    Stack (..),
    stackPush,
    stackPop,
    stackPeak
) where

data Stack a = Val a (Stack a) | Null deriving (Show, Eq)

stackPush :: a -> Stack a -> Stack a
stackPush x stack = Val x stack

stackPop :: Stack a -> Stack a
stackPop (Val _ stack) = stack
stackPop Null          = Null

stackPeak :: Stack a -> Maybe a
stackPeak (Val x _) = Just x
stackPeak Null      = Nothing


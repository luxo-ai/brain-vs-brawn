module Data where


data Stack a = Cons a (Stack a) | Empty deriving (Show, Eq)

stackPush :: a -> Stack a -> Stack a
stackPush x stack = Cons x stack

stackPop :: Stack a -> Stack a
stackPop (Cons _ stack) = stack

stackPeak :: Stack a -> a
stackPeak (Cons x _) = x

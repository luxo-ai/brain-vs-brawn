{-
For a chess board, we need to define:

1. The board
2. The players
3. The move (?)
-}
module Models.Board where

-- alias
type Name = [Char]

data Player = Player Name deriving (Show)
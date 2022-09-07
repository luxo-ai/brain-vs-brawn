{-
For a chess board, we need to define:

1. The board
2. The players
3. The move (?)
-}
module Models.Board where

-- alias
type Name = [Char]

data PieceColor = White | Black deriving (Show)
data PieceKind = King | 
                 Rook |
                 Bishop |
                 Queen |
                 Knight |
                 Pawn deriving (Show)

data Piece = Piece {
    color :: PieceColor,
    kind  :: PieceKind
} deriving (Show)


data Player = Player (Name, PieceColor) deriving (Show)

data Board = [[Maybe Piece]] deriving (Show)


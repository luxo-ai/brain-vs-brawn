module Models.Piece (Piece (..), PieceColor (..), PieceKind (..), toggleColor) where

data PieceColor = White | Black deriving (Show, Eq)

data PieceKind = King   |
                 Rook   |
                 Bishop |
                 Queen  |
                 Knight |
                 Pawn deriving (Show, Eq)

data Piece = Piece {
    pieceColor :: PieceColor,
    pieceType  :: PieceKind
} deriving (Eq)


instance Show Piece where
    show (Piece White King)   = "♔"
    show (Piece White Rook)   = "♖"
    show (Piece White Bishop) = "♗"
    show (Piece White Queen)  = "♕"
    show (Piece White Knight) = "♘"
    show (Piece White Pawn)   = "♙"
    show (Piece Black King)   = "♚"
    show (Piece Black Rook)   = "♜"
    show (Piece Black Bishop) = "♝"
    show (Piece Black Queen)  = "♛"
    show (Piece Black Knight) = "♞"
    show (Piece Black Pawn)   = "♟"

toggleColor :: PieceColor -> PieceColor
toggleColor White = Black
toggleColor Black = White

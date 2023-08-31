module Models.Piece (Piece (..), PieceColor (..), PieceKind (..), isSameColor, toggleColor) where

data PieceColor = White | Black deriving (Show, Eq)

toggleColor :: PieceColor -> PieceColor
toggleColor White = Black
toggleColor Black = White

data PieceKind = King |
                 Rook |
                 Bishop |
                 Queen |
                 Knight |
                 Pawn deriving (Show, Eq)

data Piece = Piece {
    pieceColor :: PieceColor,
    pieceType  :: PieceKind
}

instance Eq Piece where
    (Piece c1 k1) == (Piece c2 k2) = c1 == c2 && k1 == k2


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


isSameColor :: Piece -> Piece -> Bool
isSameColor (Piece c1 _) (Piece c2 _) = c1 == c2

module Models.Board (
    Cell(..),
    BoardCell,
    Board,
    showBoard,
    createBoard,
    isCellOccupiedByColor,
    isCellEmpty) where

import qualified Data.Vector  as V
import           Models.Piece (Piece (..), PieceColor (..), PieceKind (..))
import           Utils.IO     (blackTextOnWhiteBg)

data Cell piece = With piece | Empty deriving (Eq)
type BoardCell = Cell Piece

instance Functor Cell where
    fmap f (With x) = With (f x)
    fmap f Empty    = Empty

instance Applicative Cell where
    pure x = With x
    (With f) <*> (With x) = With (f x)
    _ <*> _ = Empty

instance Monad Cell where
    return = pure
    (With x) >>= f = f x
    Empty >>= _ = Empty

instance Show piece => Show (Cell piece) where
    show (With x) = show x
    show Empty    = "_"


isCellEmpty :: BoardCell -> Bool
isCellEmpty Empty = True
isCellEmpty _     = False

isCellOccupied :: BoardCell -> Bool
isCellOccupied Empty = False
isCellOccupied _     = True

isCellOccupiedByColor :: PieceColor -> BoardCell -> Bool
isCellOccupiedByColor _ Empty                  = False
isCellOccupiedByColor color (With (Piece c _)) = color == c

type BoardRow = V.Vector BoardCell
type Board = V.Vector BoardRow

createBoard :: Board
createBoard = V.fromList [
    V.fromList [
        With (Piece White Rook), With (Piece White Knight),
        With (Piece White Bishop), With (Piece White Queen),
        With (Piece White King), With (Piece White Bishop),
        With (Piece White Knight), With (Piece White Rook)
    ],
    V.fromList (replicate 8 (With (Piece White Pawn))),
    V.fromList (replicate 8 Empty),
    V.fromList (replicate 8 Empty),
    V.fromList (replicate 8 Empty),
    V.fromList (replicate 8 Empty),
    V.fromList (replicate 8 (With (Piece Black Pawn))),
    V.fromList [
        With (Piece Black Rook), With (Piece Black Knight),
        With (Piece Black Bishop), With (Piece Black Queen),
        With (Piece Black King), With (Piece Black Bishop),
        With (Piece Black Knight), With (Piece Black Rook)
    ]
 ]

showRow :: BoardRow -> String
showRow row = V.foldl' (\acc cell -> acc ++ show cell ++ "|") "|" row

showBoard :: Board -> String
showBoard board = (
    (blackTextOnWhiteBg "    A B C D E F G H    ") ++ "\n" ++
    (blackTextOnWhiteBg "   -----------------   ") ++ "\n" ++
    (blackTextOnWhiteBg $ showPieces board 1) ++
    (blackTextOnWhiteBg "   -----------------   ") ++ "\n" ++
    (blackTextOnWhiteBg "    A B C D E F G H    ")
    )
    where
        showPieces :: Board -> Int -> String
        showPieces b n
            | V.null b  = ""
            | otherwise = let row  = V.head b
                              rows = V.tail b
                          in " " ++ show n ++ " " ++ showRow row ++ " " ++ show n ++ " " ++ "\n" ++ showPieces rows (n + 1)

module Models.Board  where

import           Models.Piece

data Cell piece = With piece | Empty
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

type BoardRow = [BoardCell]
type Board = [BoardRow]

createBoard :: Board
createBoard = [
    [With (Piece White Rook),
     With (Piece White Knight),
     With (Piece White Bishop),
     With (Piece White Queen),
     With (Piece White King),
     With (Piece White Bishop),
     With (Piece White Knight),
     With (Piece White Rook)
    ],
    [With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn),
     With (Piece White Pawn)
    ],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn),
     With (Piece Black Pawn)
    ],
    [With (Piece Black Rook),
     With (Piece Black Knight),
     With (Piece Black Bishop),
     With (Piece Black Queen),
     With (Piece Black King),
     With (Piece Black Bishop),
     With (Piece Black Knight),
     With (Piece Black Rook)
    ]
 ]


showRow :: BoardRow -> String
showRow row = showRowTailRec "|" row
    where
        showRowTailRec :: String -> BoardRow -> String
        showRowTailRec acc [] = acc
        showRowTailRec acc (cell:cells) = showRowTailRec (acc ++ show cell ++ "|") cells



blackTextOnWhiteBg :: String -> String
blackTextOnWhiteBg str = "\ESC[30m\ESC[47m" ++ str ++ "\ESC[0m"



showBoard :: Board -> String
showBoard board = (
    (blackTextOnWhiteBg "    A B C D E F G H    ") ++ "\n" ++
    (blackTextOnWhiteBg "   -----------------   ") ++ "\n" ++
    (showBoard' board 1) ++
    (blackTextOnWhiteBg "   -----------------   ") ++ "\n" ++
    (blackTextOnWhiteBg "    A B C D E F G H    ")
    )
    where
        showBoard' :: Board -> Int -> String
        showBoard' [] _ = ""
        showBoard' (row:rows) n = blackTextOnWhiteBg (" " ++ show n ++ " " ++ showRow row ++ " " ++ show n ++ " ") ++ "\n" ++ showBoard' rows (n + 1)

module Moves where

import           Models.Board
import           Models.Game
import           Models.Piece

data Posn = Posn Int Int deriving (Eq, Show)
data Direction = Direction Int Int deriving (Eq, Show)

data Move = Move { from :: Posn, to :: Posn } deriving (Show)

data ErrorType = OutOfBounds | Occupied | InvalidMove | NotYourPiece | PieceDoesNotExist deriving (Eq, Show)
type WithError a = Either ErrorType a

----- CONSTANTS
boardSize :: Int
boardSize = 8

----- HELPERS
inBounds :: Posn -> Bool
inBounds (Posn x y) =
    x >= 0 &&
    x <= (boardSize - 1) &&
    y >= 0 &&
    y <= (boardSize - 1)


hasVal :: Eq a => a -> [a] -> Bool
hasVal _ [] = False
hasVal val (first:rest) = hasValTailRec False val rest where
    hasValTailRec :: Eq a => Bool -> a -> [a] -> Bool
    hasValTailRec acc val [] = acc
    hasValTailRec acc val (first:rest) = hasValTailRec (acc || (val == first)) val rest


getValAt :: Int -> [a] -> WithError a
getValAt _ []        = Left OutOfBounds
getValAt 0 (first:_) = Right first
getValAt p (_:rest)  = getValAt (p - 1) rest


setValAt :: Int -> a -> [a] -> WithError [a]
setValAt _ _ []           = Left OutOfBounds
setValAt 0 v (_:rest)     = Right (v:rest)
setValAt p v (first:rest) = (setValAt (p - 1) v rest) >>= \x -> Right (first:x)


getCellAt :: Posn -> Board -> WithError BoardCell
getCellAt (Posn x y) board = do
    row <- getValAt y board
    getValAt x row


setCellAt :: BoardCell -> Posn -> Board -> WithError Board
setCellAt cell (Posn x y) board = do
    row <- getValAt y board
    newRow <- setValAt x cell row
    setValAt y newRow board


----- FUNCTIONS
placePiece :: Piece -> Posn -> Board -> WithError Board
placePiece piece posn board = do
    cell <- getCellAt posn board
    case cell of
        Empty -> setCellAt (With piece) posn board
        _     -> Left Occupied


removePiece :: Posn -> Board -> WithError Board
removePiece posn board = do
    cell <- getCellAt posn board
    case cell of
        Empty -> Left InvalidMove
        _     -> setCellAt Empty posn board



movePiece2 :: Move -> Board -> WithError Board
movePiece2 (Move from to) board = do
    cell <- getCellAt from board
    case cell of
        Empty      -> Left InvalidMove
        -- not mutating (fn is pure) so no need to worry about
        -- undoing the move if any of the following steps fail
        With piece -> (placePiece piece to board) >>= \newBoard ->
                       removePiece from newBoard

moveActualPiece :: Piece -> Move -> Board -> WithError Board
moveActualPiece piece (Move from to) board = do
    updatedBoard <- removePiece from board
    placePiece piece to updatedBoard


movePiece :: Move -> Game -> WithError Board
movePiece move@(Move from to) game = do
    let gboard = board game
    movingPieceCell <- getCellAt from gboard
    case movingPieceCell of
        With movingPiece
            | turn game /= pieceColor movingPiece -> Left NotYourPiece
            | isValidPieceMove movingPiece move gboard -> moveActualPiece movingPiece move gboard
            | otherwise -> Left InvalidMove
        Empty -> Left PieceDoesNotExist
    where



isValidPieceMove :: Piece -> Move -> Board -> Bool
isValidPieceMove (Piece color Pawn) (Move from to) board   = to `elem` pawnMoves color from board
isValidPieceMove (Piece color Knight) (Move from to) board = to `elem` knightMoves color from board
isValidPieceMove (Piece color Bishop) (Move from to) board = to `elem` bishopMoves color from board
isValidPieceMove (Piece color Rook) (Move from to) board   = to `elem` rookMoves color from board
isValidPieceMove (Piece color Queen) (Move from to) board  = to `elem` queenMoves color from board
isValidPieceMove (Piece color King) (Move from to) board   = to `elem` kingMoves color from board


possibleMoves :: Piece -> Posn -> Board -> [Posn]
possibleMoves (Piece color Pawn)   posn board = pawnMoves color posn board
possibleMoves (Piece color Knight) posn board = knightMoves color posn board
possibleMoves (Piece color Bishop) posn board = bishopMoves color posn board
possibleMoves (Piece color Rook)   posn board = rookMoves color posn board
possibleMoves (Piece color Queen)  posn board = queenMoves color posn board
possibleMoves (Piece color King)   posn board = kingMoves color posn board


pawnMoves :: PieceColor -> Posn -> Board -> [Posn]
pawnMoves pieceColor (Posn x y) board =
    (filter isValidVertMove potentialVertMoves) ++ (filter isValidDiagMove potentialDiagMoves)
    where
        opponentColor = toggleColor pieceColor

        potentialVertMoves
            | pieceColor == White = [Posn x (y + 1)] -- Posn x (y + 2)]
            | otherwise           = [Posn x (y - 1)] -- Posn x (y - 2)]

        potentialDiagMoves
            | pieceColor == White = [Posn (x + 1) (y + 1), Posn (x - 1) (y + 1)]
            | otherwise           = [Posn (x + 1) (y - 1), Posn (x - 1) (y - 1)]

        isValidVertMove :: Posn -> Bool
        isValidVertMove posn = foldr (\cell acc -> acc || (isCellEmpty cell)) False (getCellAt posn board)

        isValidDiagMove :: Posn -> Bool
        isValidDiagMove posn = foldr (\cell acc -> acc || (isCellOccupiedByColor opponentColor cell)) False (getCellAt posn board)



knightMoves :: PieceColor -> Posn -> Board -> [Posn]
knightMoves pieceColor (Posn x y) board =
    filter isValidMove potentialMoves
    where
        opponentColor = toggleColor pieceColor

        potentialMoves = [Posn (x + 2) (y + 1), Posn (x + 2) (y - 1),
                          Posn (x - 2) (y + 1), Posn (x - 2) (y - 1),
                          Posn (x + 1) (y + 2), Posn (x + 1) (y - 2),
                          Posn (x - 1) (y + 2), Posn (x - 1) (y - 2)]

        isValidMove :: Posn -> Bool
        isValidMove posn = foldr isValidMoveAcc False (getCellAt posn board)

        isValidMoveAcc :: BoardCell -> Bool -> Bool
        isValidMoveAcc cell acc = acc || (isCellEmpty cell) || (isCellOccupiedByColor opponentColor cell)


generateSeqMoves :: PieceColor -> Posn -> Direction -> Board -> [Posn]
generateSeqMoves pieceColor (Posn x y) dir@(Direction dx dy) board
    -- not really needed but saves the linear computation of getCellAt
    | not (inBounds newPosn) = []
    -- fold over the Either from getCellAt
    | otherwise = foldr generateMovesAcc [] (getCellAt newPosn board)
    where
        newPosn = Posn (x + dx) (y + dy)
        -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
        generateMovesAcc :: BoardCell -> [Posn] -> [Posn]
        -- optimize by making this tail recursive?
        generateMovesAcc Empty _ = newPosn:(generateSeqMoves pieceColor newPosn dir board)
        generateMovesAcc (With (Piece c _)) acc
            | c /= pieceColor = newPosn : acc
            | otherwise = acc

bishopMoves :: PieceColor -> Posn -> Board -> [Posn]
bishopMoves pieceColor posn board =
    directions >>= \direction -> generateSeqMoves pieceColor posn direction board
    where
        -- NW, NE, SW, SE
        directions = [
            (Direction 1 1),
            (Direction 1 (-1)),
            (Direction (-1) 1),
            (Direction (-1) (-1))
            ]


rookMoves :: PieceColor -> Posn -> Board -> [Posn]
rookMoves pieceColor posn board =
    directions >>= \direction -> generateSeqMoves pieceColor posn direction board
    where
        -- Up, Down, Left, Right
        directions = [
            (Direction 1 0),
            (Direction (-1) 0),
            (Direction 0 1),
            (Direction 0 (-1))
            ]


queenMoves :: PieceColor -> Posn -> Board -> [Posn]
queenMoves pieceColor posn board =
    (rookMoves pieceColor posn board) ++ (bishopMoves pieceColor posn board)


kingMoves :: PieceColor -> Posn -> Board -> [Posn]
kingMoves pieceColor (Posn x y) board =
    filter isValidMove potentialMoves
    where
        opponentColor = toggleColor pieceColor

        potentialMoves = [Posn (x + 1) y, Posn (x - 1) y,
                          Posn x (y + 1), Posn x (y - 1),
                          Posn (x + 1) (y + 1), Posn (x - 1) (y - 1),
                          Posn (x + 1) (y - 1), Posn (x - 1) (y + 1)]

        isValidMove :: Posn -> Bool
        isValidMove posn = foldr isValidMoveAcc False (getCellAt posn board)

        isValidMoveAcc :: BoardCell -> Bool -> Bool
        isValidMoveAcc cell acc = acc || (isCellEmpty cell) || (isCellOccupiedByColor opponentColor cell)


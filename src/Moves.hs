module Moves where

import           Models.Board
import           Models.Piece


data Posn = Posn Int Int deriving (Eq, Show)
data Move = Move { from :: Posn, to :: Posn } deriving (Show)

data ErrorType = OutOfBounds | Occupied | InvalidMove deriving (Eq, Show)
type WithError a = Either ErrorType a


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

movePiece :: Move -> Board -> WithError Board
movePiece move@(Move from to) board = do
    movingPieceCell <- getCellAt from board
    case movingPieceCell of
        Empty -> Left InvalidMove
        With movingPiece ->
            if isValidPieceMove movingPiece move board
               then moveActualPiece movingPiece move board
               else Left InvalidMove

isValidPieceMove :: Piece -> Move -> Board -> Bool
isValidPieceMove (Piece _ Pawn) (Move from to) board   = to `elem` pawnMoves from board
isValidPieceMove (Piece _ Knight) (Move from to) board = to `elem` knightMoves from board
isValidPieceMove (Piece _ Bishop) (Move from to) board = to `elem` bishopMoves from board
isValidPieceMove (Piece _ Rook) (Move from to) board   = to `elem` rookMoves from board
isValidPieceMove (Piece _ Queen) (Move from to) board  = to `elem` queenMoves from board
isValidPieceMove (Piece _ King) (Move from to) board   = to `elem` kingMoves from board

moveActualPiece :: Piece -> Move -> Board -> WithError Board
moveActualPiece piece (Move from to) board = do
    updatedBoard <- removePiece from board
    placePiece piece to updatedBoard


getMoves :: Posn -> Board -> [Posn]
getMoves posn board = case (getCellAt posn board) of
    Left _ -> []
    Right (With piece) -> case (pieceType piece) of
        Pawn   -> pawnMoves posn board
        Knight -> knightMoves posn board
        Bishop -> bishopMoves posn board
        Rook   -> rookMoves posn board
        Queen  -> queenMoves posn board
        King   -> kingMoves posn board
        _      -> []

possibleMoves :: Piece -> Posn -> Board -> [Posn]
possibleMoves (Piece _ Pawn)   posn board = pawnMoves posn board
possibleMoves (Piece _ Knight) posn board = knightMoves posn board
possibleMoves (Piece _ Bishop) posn board = bishopMoves posn board
possibleMoves (Piece _ Rook)   posn board = rookMoves posn board
possibleMoves (Piece _ Queen)  posn board = queenMoves posn board
possibleMoves (Piece _ King)   posn board = kingMoves posn board


isMoveValid :: Posn -> Board -> Bool
isMoveValid posn board =
    case getCellAt posn board of
        Right Empty              -> True
        Right (With (Piece _ _)) -> False
        Left _                   -> False


pawnMoves :: Posn -> Board -> [Posn]
pawnMoves posn@(Posn x y) board =
    case getCellAt posn board of
        Right (With (Piece color Pawn)) -> filter validMove moves
            where
                moves = if color == White then [Posn x (y + 1), Posn x (y + 2)] else [Posn x (y - 1), Posn x (y - 2)]
                validMove movePosn = case getCellAt movePosn board of
                    Right Empty              -> True
                    Right (With (Piece c _)) -> c /= color
                    Left _                   -> False
        _ -> []


knightMoves :: Posn -> Board -> [Posn]
knightMoves posn@(Posn x y) board =
    filter validMove potentialMoves
    where
        potentialMoves = [Posn (x + 2) (y + 1), Posn (x + 2) (y - 1),
                          Posn (x - 2) (y + 1), Posn (x - 2) (y - 1),
                          Posn (x + 1) (y + 2), Posn (x + 1) (y - 2),
                          Posn (x - 1) (y + 2), Posn (x - 1) (y - 2)]

        inBounds (Posn x y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

        validMove movePosn =
            inBounds movePosn && case getCellAt movePosn board of
                Right Empty                -> True
                Right (With (Piece c _))   -> case getCellAt posn board of
                                                Right (With (Piece knightColor _)) -> knightColor /= c
                                                _ -> False
                Left _                     -> False



bishopMoves :: Posn -> Board -> [Posn]
bishopMoves posn@(Posn x y) board =
    concatMap (\direction -> expandMove direction posn) directions
    where
        directions = [(1,1), (1,-1), (-1,1), (-1,-1)] -- NW, NE, SW, SE

        inBounds (Posn x y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

        expandMove (dx, dy) (Posn x y)
            | not (inBounds newPosn) = []
            | otherwise = case getCellAt newPosn board of
                Right Empty -> newPosn : expandMove (dx, dy) newPosn
                Right (With (Piece c _)) -> case getCellAt posn board of
                    Right (With (Piece bishopColor _))
                        | bishopColor /= c -> [newPosn]  -- Opponent's piece; can capture it but can't move past it.
                        | otherwise -> []  -- Own piece; can't move to or past this square.
                    _ -> []
                Left _ -> []
            where newPosn = Posn (x + dx) (y + dy)


rookMoves :: Posn -> Board -> [Posn]
rookMoves posn@(Posn x y) board =
    concatMap (\direction -> expandMove direction posn) directions
    where
        directions = [(1,0), (-1,0), (0,1), (0,-1)] -- Up, Down, Left, Right

        inBounds (Posn x y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

        expandMove (dx, dy) (Posn x y)
            | not (inBounds newPosn) = []
            | otherwise = case getCellAt newPosn board of
                Right Empty -> newPosn : expandMove (dx, dy) newPosn
                Right (With (Piece c _)) -> case getCellAt posn board of
                    Right (With (Piece rookColor _))
                        | rookColor /= c -> [newPosn]  -- Opponent's piece; can capture it but can't move past it.
                        | otherwise -> []  -- Own piece; can't move to or past this square.
                    _ -> []
                Left _ -> []
            where newPosn = Posn (x + dx) (y + dy)


queenMoves :: Posn -> Board -> [Posn]
queenMoves posn board =
    (rookMoves posn board) ++ (bishopMoves posn board)



kingMoves :: Posn -> Board -> [Posn]
kingMoves (Posn x y) board = filter inBounds [
    Posn (x + 1) (y + 1), Posn (x + 1) (y - 1),
    Posn (x - 1) (y + 1), Posn (x - 1) (y - 1),
    Posn (x + 1) y,       Posn (x - 1) y,
    Posn x (y + 1),      Posn x (y - 1),
    Posn (x) y]
    where
        inBounds :: Posn -> Bool
        inBounds (Posn x y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

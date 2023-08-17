module Moves where

import Models.Board
import Models.Piece


data Posn = Posn Int Int deriving (Eq, Show)
type Delta = Int


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
        _     -> setCellAt Empty posn boards
    

movePiece :: Posn -> Posn -> Board -> WithError Board
movePiece from to board = do
    cell <- getCellAt from board
    case cell of
        Empty -> Left InvalidMove
        -- not mutating (fn is pure) so no need to worry about 
        -- undoing the move if any of the following steps fail
        _     -> placePiece cell to board >>= \newBoard ->
                 removePiece from newBoard

                 

{-
placePiece :: Piece -> Posn -> Board -> Maybe Board
placePiece piece posn board = do
    cell <- getCellAt posn board
    case cell of
        Empty -> setCellAt (With piece) posn board
        _     -> Nothing




-}


blackPawnMoves :: Posn -> [Posn]
blackPawnMoves (Posn x y) = [Posn x (y - 1)]

whitePawnMoves :: Posn -> [Posn]
whitePawnMoves (Posn x y) = [Posn x (y + 1)]

blackPawnAttackMoves :: Posn -> [Posn]
blackPawnAttackMoves (Posn x y) = [Posn (x - 1) (y - 1), Posn (x + 1) (y - 1)]

whitePawnAttackMoves :: Posn -> [Posn]
whitePawnAttackMoves (Posn x y) = [Posn (x - 1) (y + 1), Posn (x + 1) (y + 1)]

{-

moveWhitePawn :: Posn -> Board -> Maybe Board
moveWhitePawn posn board = let 
    attackMoves = whitePawnAttackMoves posn
    generalMoves = whitePawnMoves posn
    in
        do
            cell <- getCellAt posn board
            case cell of
                With (Piece White _) -> Nothing
                With (Piece Black _) -> Nothing
                Empty -> if hasVal posn (generalMoves) then
                    setCellAt Empty posn board >>= \newBoard ->
                    setCellAt (With (Piece White Pawn)) (head generalMoves) newBoard
                    else 
                        Nothing




                    if hasVal (Posn x y) (attackMoves ++ generalMoves) then
                        setCellAt (BoardCell Empty) (Posn x y) board >>= \newBoard ->
                        setCellAt (BoardCell (Piece White Pawn)) (head generalMoves) newBoard
                    else
                        Nothing
                _ -> Nothing





        do
    cell <- getCellAt (Posn x y) board
    case cell of
        (BoardCell (Piece White Pawn)) -> do
            let moves = whitePawnMoves (Posn x y)
            if hasVal (Posn x y) moves then
                setCellAt (BoardCell Empty) (Posn x y) board >>= \newBoard ->
                setCellAt (BoardCell (Piece White Pawn)) (head moves) newBoard
            else
                Nothing
        _ -> Nothing



movePawnFactory :: PieceColor -> (Posn -> Board -> Maybe Board)
movePawnFactory color = mvPawnFn where 
    delta = case color of
        White -> 1
        Black -> -1

    mvPawnFn:: Posn -> Board -> Maybe Board
    mvPawnFn (Posn x y) board = let 
        attackMoves = [
            Posn (x - 1) (y + delta), 
            Posn (x + 1) (y + delta)
            ]
        generalMoves = [
            Posn x (y + delta)
            ]
        in
            case () of 
                _ | hasVal (Posn x y) (attackMoves ++ generalMoves) -> Nothing
                  | otherwise -> Nothing


-}

{-



setPiece :: Piece -> Posn -> Board -> MoveResult2
setPiece piece to [] = Right []
setPiece piece (Posn x y) board = 
    





setValAt y (setValAt x piece (board !! y)) board
    


case () of
    _ | x < (length board) = (>>= ) Right 
      | otherwise          = Left OutOfBounds
    






updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 newValue (_:xs) = newValue : xs
updateAt pos newValue (x:xs) = x : updateAt (pos - 1) newValue xs

pieceColorDelta :: PieceColor -> Int
pieceColorDelta color = case color of
    White -> 1
    Black -> -1




pawnMoves2 :: Posn -> Board -> Board
pawnMoves2 (Posn x y) board = fmap ()
-}

{-
moves :: Piece -> Posn -> [Posn]
moves (Piece color kind) currentPosn = case kind of
    Pawn   -> pawnMoves (pieceColorDelta color) currentPosn
    Knight -> knightMoves currentPosn
    Bishop -> bishopMoves currentPosn
    Rook   -> rookMoves currentPosn
    Queen  -> queenMoves currentPosn
    King   -> kingMoves currentPosn

pawnMoves :: Int -> Posn -> [Posn]
pawnMoves delta (Posn x y) = [Posn (x, y + delta)]

knightMoves :: Posn -> [Posn]
knightMoves (Posn x y) = [Posn (x + 2, y + 1), Posn (x + 2, y - 1),
                             Posn (x - 2, y + 1), Posn (x - 2, y - 1),
                             Posn (x + 1, y + 2), Posn (x + 1, y - 2),
                             Posn (x - 1, y + 2), Posn (x - 1, y - 2)]

bishopMoves :: Posn -> [Posn]
bishopMoves (Posn x y) = [Posn (x + n, y + n) | n <- [-7..7], n /= 0] ++
                           [Posn (x + n, y - n) | n <- [-7..7], n /= 0]

rookMoves :: Posn -> [Posn]
rookMoves (Posn x y) = [Posn (x + n, y) | n <- [-7..7], n /= 0] ++
                          [Posn (x, y + n) | n <- [-7..7], n /= 0]

queenMoves :: Posn -> [Posn]
queenMoves (Posn x y) = [Posn (x + n, y + n) | n <- [-7..7], n /= 0] ++
                           [Posn (x + n, y - n) | n <- [-7..7], n /= 0] ++
                           [Posn (x + n, y) | n <- [-7..7], n /= 0] ++
                           [Posn (x, y + n) | n <- [-7..7], n /= 0]
                        
kingMoves :: Posn -> [Posn]
kingMoves (Posn x y) = [Posn (x + 1, y + 1), Posn (x + 1, y - 1),
                           Posn (x - 1, y + 1), Posn (x - 1, y - 1),
                           Posn (x + 1, y), Posn (x - 1, y),
                           Posn (x, y + 1), Posn (x, y - 1)]





-}
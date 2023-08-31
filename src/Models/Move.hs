module Models.Move where


import           Models.Board
import           Models.Piece
import           Utils.Safe

data Posn = Posn Int Int deriving (Eq, Show, Ord)
data Direction = Direction Int Int deriving (Eq, Show)

data Move = Move { from :: Posn, to :: Posn } deriving (Show)

data ErrorType = OutOfBounds | Occupied | InvalidMove | NotYourPiece | PieceDoesNotExist deriving (Eq, Show)
type WithError a = Either ErrorType a


boardSize :: Int
boardSize = 8

lastBoardIndex :: Int
lastBoardIndex = boardSize - 1


inBounds :: Posn -> Bool
inBounds (Posn x y) =
    x >= 0 &&
    x <= lastBoardIndex &&
    y >= 0 &&
    y <= lastBoardIndex


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


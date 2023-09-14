module Models.Move (
    Move (..),
    Direction (..),
    getCellAt,
    setCellAt,
    lastBoardIndex,
    inBounds,
    ) where


import           Models.Board
import           Models.Error (ErrorType (..), WithError (..))
import           Models.Piece
import           Models.Posn  (Posn (..))
import           Utils.Safe

import qualified Data.Vector  as V

data Direction = Direction Int Int deriving (Eq, Show)

data Move = Move {
    from :: Posn,
    to   :: Posn
    } deriving (Eq)

instance Show Move where
    show (Move from to) = show from ++ " -> " ++ show to

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


getValAt :: Int -> V.Vector a -> WithError a
getValAt idx vec = case vec V.!? idx of
    Just val -> Right val
    Nothing  -> Left OutOfBounds

setValAt :: Int -> a -> V.Vector a -> WithError (V.Vector a)
setValAt idx val vec = case vec V.!? idx of
    Just _  -> Right (vec V.// [(idx, val)])
    Nothing -> Left OutOfBounds


getCellAt :: Posn -> Board -> WithError BoardCell
getCellAt (Posn x y) board = do
    row <- getValAt y board
    getValAt x row


setCellAt :: BoardCell -> Posn -> Board -> WithError Board
setCellAt cell (Posn x y) board = do
    row <- getValAt y board
    newRow <- setValAt x cell row
    setValAt y newRow board


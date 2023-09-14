module Models.Player (Player (..)) where

import           Models.Piece (Piece, PieceColor)
import           Models.Posn  (WithPosn (..))

data Player = Player {
    playerName  :: String,
    playerColor :: PieceColor,
    score       :: Int,
    pieces      :: [WithPosn Piece]
} deriving (Eq)


instance Show Player where
    show (Player name color score _) = name ++ " [" ++ show color ++ " - " ++ show score ++ "] "

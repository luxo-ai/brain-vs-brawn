module Models.Player (Player (..)) where

import           Models.Piece (PieceColor)

data Player = Player {
    playerName  :: String,
    playerColor :: PieceColor,
    score       :: Int
} deriving (Eq)

instance Show Player where
    show (Player name color score) = name ++ " [" ++ show color ++ " - " ++ show score ++ "] "

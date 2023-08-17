module Models.Player where

import           Models.Piece

data Player = Player {
    playerName  :: String,
    playerColor :: PieceColor,
    score       :: Int
}

instance Show Player where
    show (Player name color score) = name ++ " [" ++ show color ++ " - " ++ show score ++ "] "

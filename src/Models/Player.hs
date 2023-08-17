module Models.Player where

import Models.Piece

data Player = Player {
    playerName :: String,
    playerColor :: PieceColor
}

instance Show Player where
    show (Player name color) = name ++ " (" ++ show color ++ ")"
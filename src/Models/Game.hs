module Models.Game where

import Models.Board (Board)
import Models.Player (Player)


data Game = Game {
    turn  :: Player,
    board :: Board
} deriving (Show)


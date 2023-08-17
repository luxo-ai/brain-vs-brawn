module Models.Game where

import           Models.Board  (Board)
import           Models.Piece
import           Models.Player (Player)

data Game = Game {
    black :: Player,
    white :: Player,
    turn  :: PieceColor,
    board :: Board
} deriving (Show)

turnPlayer :: Game -> Player
turnPlayer game = case (turn game) of
    White -> white game
    _     -> black game

togglePlayer :: Game -> Game
togglePlayer game = case (turn game) of
    White -> game { turn = Black }
    _     -> game { turn = White }

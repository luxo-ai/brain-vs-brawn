module Models.Game where

import           Helpers
import           Models.Board  (Board, createBoard)
import           Models.Move
import           Models.Piece
import           Models.Player
-- (Player, playerColor, score)

data Game = Game {
    player1 :: Player,
    player2 :: Player,
    turn    :: PieceColor,
    board   :: Board,
    history :: Stack Move
} deriving (Show)

getTurnPlayer :: Game -> Player
getTurnPlayer game = getPlayerForColor (turn game) game

getPlayerForColor :: PieceColor -> Game -> Player
getPlayerForColor color (Game p1 p2 _ _ _) = if color == (playerColor p1) then p1 else p2

toggleTurn :: Game -> Game
toggleTurn game = case (turn game) of
    White -> game { turn = Black }
    _     -> game { turn = White }


incCurrentPlayerScore :: Game -> Game
incCurrentPlayerScore game = game { player1 = p1', player2 = p2' }
    where
        currentTurn = turn game
        p1          = player1 game
        p2          = player2 game
        p1ScoreIncr = if currentTurn == (playerColor p1) then 1 else 0
        p2ScoreIncr = if currentTurn == (playerColor p2) then 1 else 0
        p1'         = p1 { score = p1ScoreIncr }
        p2'         = p2 { score = p2ScoreIncr }


initialGame :: Player -> Player -> Game
initialGame player1 player2 = Game {
    player1 = player1,
    player2 = player2,
    turn    = White,
    board   = createBoard,
    history = Null
}

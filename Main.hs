module Main (main) where

import Moves
import Models.Player
import Models.Board
import Models.Game
import Models.Piece
import Data


player = Player "Luis" White
game = Game player createBoard


getValueIO :: Maybe a -> a
getValueIO (Just x) = x
getValueIO Nothing  = error "No value inside Maybe!"


printGame :: Game -> IO ()
printGame (Game player board) = do
    putStrLn ""
    putStrLn . (\x -> "TURN: " ++ x) $ show player
    putStrLn $ showBoard board
    putStrLn ""    


main :: IO ()
main =  printGame game

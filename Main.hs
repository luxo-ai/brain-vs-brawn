{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Text.Regex.PCRE

import           Data
import           Models.Board
import           Models.Game
import           Models.Piece
import           Models.Player
import           Moves
import           Utils.Safe


player1 = Player "luxo" Black 0
player2 = Player "ai" White 0

game = Game {
    black = player1,
    white = player2,
    turn  = White,
    board = createBoard
}


parsePattern :: String
parsePattern = "([a-hA-H])([1-8])(?:\\s*(?:->|to|=>|:)\\s*)([a-hA-H])([1-8])"

parseMove :: String -> Maybe Move
parseMove s =
    if s =~ parsePattern then
        let (_, _, _, [x1, y1, x2, y2]) = s =~ parsePattern :: (String, String, String, [String])
        in do
            x1' <- (maybeHead x1) >>= charToDigit
            x2' <- (maybeHead x2) >>= charToDigit
            y1' <- maybeRead y1 :: Maybe Int
            y2' <- maybeRead y2 :: Maybe Int
            p1  <- Just $ Posn (x1' - 1) (y1' - 1)
            p2  <- Just $ Posn (x2' - 1) (y2' - 1)
            return (Move p1 p2)
    else Nothing


makeMove :: Move -> Game -> WithError Game
makeMove move game = do
    let gameBoard = board game
    updatedBoard <- movePiece move (gameBoard)
    return $ game { board = updatedBoard, turn = toggleColor $ turn game }


printGame :: Game -> IO ()
printGame game = do
    putStrLn ""
    putStrLn $ show $ turnPlayer game
    putStrLn $ showBoard $ board game
    putStrLn ""


runGame :: Game -> IO ()
runGame game = do
    printGame game
    putStr "Enter a move: "
    move <- getLine
    case (parseMove move) of
        Just parsedMove -> do
            case (makeMove parsedMove game) of
                Left err -> do
                    putStrLn $ show err
                    runGame game
                Right newGame -> do
                    runGame newGame
        Nothing -> do
            putStrLn "Unkown move format"
            runGame game

main :: IO ()
main = runGame game

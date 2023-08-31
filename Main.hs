{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Text.Regex.PCRE

import           AI
import           Models.Board
import           Models.Game
import           Models.Move
import           Models.Piece
import           Models.Player
import           Moves
import           Utils.IO
import           Utils.Safe


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



printGame :: Game -> IO ()
printGame game = do
    putStrLn ""
    putStrLn $ show $ getTurnPlayer game
    putStrLn $ showBoard $ board game
    putStrLn ""


withErrorStr :: String -> String
withErrorStr str = "\ESC[31m" ++ "ERROR!!! " ++ str ++ "\ESC[0m"


type ErrorString = String
printMaybeError :: Maybe ErrorString -> IO ()
printMaybeError (Just err) = putStrLn err
printMaybeError Nothing    = return ()

runGame :: Game -> Maybe String -> IO ()
runGame game maybeError = do
    clearScreen
    printMaybeError $ withErrorStr <$> maybeError
    printGame game
    putStrLn "Enter a move: "
    move <- getLine
    case (parseMove move) of
        Just parsedMove -> do
            case (movePiece parsedMove game) of
                Left err -> do
                    runGame game (Just $ show err)
                Right newGame -> do
                    runGame newGame Nothing
        Nothing -> do
            runGame game (Just "Invalid move")


runGame2 :: Game -> Maybe String -> IO ()
runGame2 game maybeError = do
    clearScreen
    printGame game
    putStrLn "KB: "
    case (movePiece (miniMax game) game) of
        Left err -> do
            runGame2 game (Just $ show err)
        Right newGame -> do
            runGame2 newGame Nothing

start :: Game
start = (initialGame (Player "ai" White 0) (Player "luxo" Black 0))

main :: IO ()
main = runGame2 (initialGame (Player "ai" White 0) (Player "luxo" Black 0)) Nothing

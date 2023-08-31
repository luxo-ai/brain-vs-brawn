module TicTacToe where

data Player = X | O deriving (Show, Eq)
-- 2 x 2 for simplicity
data TicTacToeBoard = TicTacToeBoard {
    topLeft     :: Maybe Player,
    topRight    :: Maybe Player,
    bottomLeft  :: Maybe Player,
    bottomRight :: Maybe Player
}

data TicTacToe = TicTacToe {
    board :: TicTacToeBoard,
    turn  :: Player
} deriving (Show)

data Move = TopLeft | TopRight | BottomLeft | BottomRight deriving (Show)

togglePlayer :: Player -> Player
togglePlayer X = O
togglePlayer O = X

isGameOver :: TicTacToe -> Bool
isGameOver (TicTacToe board _)
    | winCondition (topLeft board)     (topRight board)     = True
    | winCondition (bottomLeft board)  (bottomRight board)  = True
    | winCondition (topLeft board)     (bottomLeft board)   = True
    | winCondition (topRight board)    (bottomRight board)  = True
    | winCondition (topLeft board)     (bottomRight board)  = True
    | winCondition (topRight board)    (bottomLeft board)   = True
    | isDraw board                                          = True
    | otherwise                                             = False
  where
    winCondition a b = a == b && a /= Nothing
    isDraw b = all (/= Nothing) [topLeft b, topRight b, bottomLeft b, bottomRight b]


placeMove :: Move -> TicTacToe -> TicTacToe
placeMove move (TicTacToe board turn) =
    case move of
        TopLeft     -> TicTacToe (board { topLeft = Just turn }) nextPlayer
        TopRight    -> TicTacToe (board { topRight = Just turn }) nextPlayer
        BottomLeft  -> TicTacToe (board { bottomLeft = Just turn }) nextPlayer
        BottomRight -> TicTacToe (board { bottomRight = Just turn}) nextPlayer
    where
      nextPlayer = togglePlayer turn


possibleGames :: TicTacToe -> [TicTacToe]
possibleGames ticTacToe = fmap (\move -> placeMove move ticTacToe) (possibleMoves ticTacToe)

possibleMoves :: TicTacToe -> [Move]
possibleMoves ticTacToe = filter isValidMove [TopLeft, TopRight, BottomLeft, BottomRight]
  where
    isValidMove move =
        case move of
            TopLeft     -> topLeft (board ticTacToe) == Nothing
            TopRight    -> topRight (board ticTacToe) == Nothing
            BottomLeft  -> bottomLeft (board ticTacToe) == Nothing
            BottomRight -> bottomRight (board ticTacToe) == Nothing


scoreGame :: TicTacToe -> Int
scoreGame (TicTacToe board _)
    | winCondition (topLeft board) (topRight board)       = scoreForWinner (topLeft board)
    | winCondition (bottomLeft board) (bottomRight board) = scoreForWinner (bottomLeft board)
    | winCondition (topLeft board) (bottomLeft board)     = scoreForWinner (topLeft board)
    | winCondition (topRight board) (bottomRight board)   = scoreForWinner (topRight board)
    | winCondition (topLeft board) (bottomRight board)    = scoreForWinner (topLeft board)
    | winCondition (topRight board) (bottomLeft board)    = scoreForWinner (topRight board)
    | otherwise                                           = partialScore board
  where
    winCondition a b = a == b && a /= Nothing
    scoreForWinner (Just X) = 10
    scoreForWinner (Just O) = -10
    scoreForWinner Nothing  = 0
    partialScore b = sum $ map scoreLine [
        (topLeft b, topRight b),
        (bottomLeft b, bottomRight b),
        (topLeft b, bottomLeft b),
        (topRight b, bottomRight b),
        (topLeft b, bottomRight b),
        (topRight b, bottomLeft b)]
    scoreLine (Just X, Nothing) = 5
    scoreLine (Nothing, Just X) = 5
    scoreLine (Just O, Nothing) = -5
    scoreLine (Nothing, Just O) = -5
    scoreLine _                 = 0


instance Show TicTacToeBoard where
    show board =
        unlines [ showCell (topLeft board)     ++ "|" ++ showCell (topRight board)
                , "---"
                , showCell (bottomLeft board)  ++ "|" ++ showCell (bottomRight board)
                ]
      where
        showCell Nothing  = " "
        showCell (Just X) = "X"
        showCell (Just O) = "O"


printTicTacToes :: [TicTacToe] -> IO ()
printTicTacToes ticTacToes = printTicTacToes' ticTacToes 1
    where
        printTicTacToes' :: [TicTacToe] -> Int -> IO ()
        printTicTacToes' [] _ = do
            putStrLn "-------------"
        printTicTacToes' (t:ts) line = do
            putStrLn $ "\n" ++ show (board t)
            printTicTacToes' ts (line + 1)

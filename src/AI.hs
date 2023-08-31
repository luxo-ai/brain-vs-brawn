module AI where

import           Data.List    (maximumBy, minimumBy)
import           Data.Ord     (comparing)

import           Models.Board
import           Models.Error (WithError (..))
import           Models.Game
import           Models.Move
import           Models.Piece
import           Moves


{-

Max                g0
                 |    |
Min     g1 (b=0 from prev sub tree)   g2
             |            |
       a=-inf, 1
Max    b=0 g3 g4       g5  g6
        |  |  |  |   |  |  |  |
Min    g7 g8 g9 g10  g11 g12 g13 g14
        1 7  9   2    3   4   5   6

-}


type Depth = Int

data Score = Score {
    val    :: Int,
    pruned :: Bool
}

instance Show Score where
    show (Score v p) = "Score " ++ (show v) ++ " " ++ (show p)

instance Eq Score where
    (Score v1 _) == (Score v2 _) = v1 == v2

instance Ord Score where
    (Score v1 _) <= (Score v2 _) = v1 <= v2

data Player = Max | Min deriving (Eq, Show)

-- clean up
expandGame :: Game -> [Game]
expandGame game = case (unwrap (fmap (\move -> movePiece move game) allMovesForGame)) of
    Nothing -> error "ERRRRRRRRRRRR"
    Just g  -> g
    where
        allMovesForGame  = findAllMoves game
        unwrap :: [WithError Game] -> Maybe [Game]
        unwrap []             = Just []
        unwrap ((Left _):_)   = Nothing
        unwrap ((Right g):gs) = (:) <$> Just g <*> unwrap gs


data AlphaBetaArgs a = AlphaBetaArgs {
    game  :: a,
    depth :: Int,
    alpha :: Score,
    beta  :: Score,
    isMax :: Bool
}



alphaBeta2 :: (a -> Bool) -> (a -> Score) -> (a -> [a]) -> AlphaBetaArgs a -> Score
alphaBeta2 isOver scoreGame expandGame args@(AlphaBetaArgs game depth alpha beta isMax)
    | depth == 0 || isOver game = scoreGame game
    | isMax                     = foldr findAlpha alpha (expandGame game)
    | otherwise                 = foldr findBeta beta (expandGame game)
    where
        findAlpha :: a -> Score -> Score
        findAlpha game score
            -- skip if already pruned
            | pruned score     = score
            | newAlpha >= beta = newAlpha { pruned = True }
            | otherwise        = newAlpha
            where
                beta'    = alphaBeta2 isOver scoreGame expandGame args { depth = (depth - 1), alpha = score, isMax = False }
                newAlpha = max score beta'

        findBeta :: a -> Score -> Score
        findBeta game score
            | pruned score     = score
            | newBeta <= alpha = newBeta { pruned = True }
            | otherwise        = newBeta
            where
                alpha'   = alphaBeta2 isOver scoreGame expandGame args { depth = (depth - 1), beta = score, isMax = True }
                newBeta  = min score beta

alphaBeta :: Game -> Depth -> Score -> Score -> Player -> Score
alphaBeta game depth alpha beta minMaxPlayer
    | depth == 0 || isGameOver game = evaluate game
    | minMaxPlayer == Max           = foldr findAlpha alpha (expandGame game) -- unprune
    | otherwise                     = foldr findBeta beta (expandGame game)   -- unprune
    where
        findAlpha :: Game -> Score -> Score
        findAlpha game score
            -- skip if already pruned
            | pruned score     = score
            | newAlpha >= beta = newAlpha { pruned = True }
            | otherwise        = newAlpha
            where
                beta'    = alphaBeta game (depth - 1) score beta Min
                newAlpha = max score beta'

        findBeta :: Game -> Score -> Score
        findBeta game score
            | pruned score     = score
            | newBeta <= alpha = newBeta { pruned = True }
            | otherwise        = newBeta
            where
                alpha'   = alphaBeta game (depth - 1) alpha score Max
                newBeta  = min score beta



evaluate :: Game -> Score
evaluate (Game _ _ turn board _) = Score (evaluateBoard board) False
    where
        evaluateBoard :: Board -> Int
        evaluateBoard = foldr (\row acc -> acc + (evaluateRow row)) 0
            where
                evaluateRow = foldr (\cell acc -> acc + (evaluateCell cell)) 0

                evaluateCell :: BoardCell -> Int
                evaluateCell Empty = 0
                evaluateCell (With (Piece color pieceType)) =
                    let value = pieceValue pieceType in
                    if color == White then value else (-value)

                pieceValue :: PieceKind -> Int
                pieceValue Pawn   = 1
                pieceValue Knight = 3
                pieceValue Bishop = 3
                pieceValue Rook   = 5
                pieceValue Queen  = 9
                pieceValue King   = 50  -- We give a high value to the king, although in actual games, the king's loss means the end of the game.

-- https://www.baeldung.com/cs/minimax-algorithm
initialAlpha :: Score
initialAlpha = Score (-10000) False

initialBeta :: Score
initialBeta = Score 10000 False

miniMax :: Game -> Move
miniMax game@(Game _ _ White board _) = maximumBy (comparing $ alphaBetaForMove) possibleMoves
    where
        possibleMoves = findAllMoves game
        alphaBetaForMove :: Move -> Score
        alphaBetaForMove move = case movePiece move game of
            Left _  -> error "TODO: handle error in pure way"
            Right g -> alphaBeta g 300 initialAlpha initialBeta Min

miniMax game@(Game _ _ Black board _) = minimumBy (comparing $ alphaBetaForMove) possibleMoves
    where
        possibleMoves = findAllMoves game
        alphaBetaForMove :: Move -> Score
        alphaBetaForMove move = case movePiece move game of
            Left _  -> error "TODO: handle error in pure way"
            Right g -> alphaBeta g 300 initialAlpha initialBeta Max

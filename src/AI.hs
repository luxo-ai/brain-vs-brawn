module AI where

import           Data.List    (maximumBy, minimumBy)
import           Data.Ord     (comparing)
import           Models.Board
import           Models.Error (WithError (..))
import           Models.Game
import           Models.Move
import           Models.Piece
import           Moves

data Score = Score {
    val    :: Int,
    pruned :: Bool
} deriving (Eq, Show)

instance Ord Score where
    compare (Score v1 _) (Score v2 _) = compare v1 v2

data AlphaBetaArgs a = AlphaBetaArgs {
    game  :: a,
    depth :: Int,
    alpha :: Score,
    beta  :: Score,
    isMax :: Bool
}

alphaBeta :: (a -> Bool) -> (a -> Score) -> (a -> [a]) -> AlphaBetaArgs a -> Score
alphaBeta isOver scoreGame expandGame args@(AlphaBetaArgs ga depth alpha beta isMax)
    | depth == 0 || isOver ga = scoreGame ga
    | isMax                     = foldl findAlpha alpha (expandGame ga)
    | otherwise                 = foldl findBeta beta (expandGame ga)
    where
        findAlpha score g
            -- skip if already pruned
            | pruned score     = score
            | newAlpha >= beta = newAlpha { pruned = True }
            | otherwise        = newAlpha
            where
                beta'    = alphaBeta isOver scoreGame expandGame args { game = g, depth = (depth - 1), alpha = score, isMax = False }
                newAlpha = max score beta'

        findBeta score g
            | pruned score     = score
            | newBeta <= alpha = newBeta { pruned = True }
            | otherwise        = newBeta
            where
                alpha'   = alphaBeta isOver scoreGame expandGame args { game = g, depth = (depth - 1), beta = score, isMax = True }
                newBeta  = min score alpha'

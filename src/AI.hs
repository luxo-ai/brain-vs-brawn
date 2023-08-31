module AI where

import qualified Data.Set     as Set


import           Data.List    (maximumBy, minimumBy)
import           Data.Ord     (comparing)

import           Models.Board
import           Models.Game
import           Models.Move
import           Models.Piece
import           Moves


isInCheckMate :: PieceColor -> Game -> Bool
isInCheckMate color (Game _ _ _ board _) = maybe False isInCheckMate' (maybeFindPiece kingPiece board)
    where
        kingPiece = Piece color King

        allOpponentMoves = fmap to (findAllMovesForColor (toggleColor color) board)
        -- Set is a self-balancing binary search tree. Lookup is O(log n)
        allOpponentMovesLookup = Set.fromList allOpponentMoves

        isInCheck :: Posn -> Bool
        isInCheck p = Set.member p allOpponentMovesLookup

        isInCheckMate' :: Posn -> Bool
        isInCheckMate' p = isInCheck p && all (\move -> isInCheck move) (kingMoves color p board)


isGameOver :: Game -> Bool
isGameOver game = isInCheckMate White game || isInCheckMate Black game

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

data MinMaxPlayer = Max | Min deriving (Eq, Show)


{-
    for each game in newGames
        beta' = alphaBeta game (depth - 1) alpha beta Min
        newAlpha = max alpha beta'
        if newAlpha >= beta
            return newAlpha
        else continue
-}

-- get next possible games
expandGame :: Game -> [Game]
expandGame game = case unwrap expandedWithErrors of
    Nothing -> []
    Just gs -> gs
    where
        expandedWithErrors = fmap (\move -> movePiece move game) (findAllMoves game)
        unwrap :: [WithError Game] -> Maybe [Game]
        unwrap []             = Just []
        unwrap ((Left _):_)   = Nothing
        unwrap ((Right g):gs) = (:) <$> Just g <*> unwrap gs

alphaBeta :: Game -> Depth -> Score -> Score -> MinMaxPlayer -> Score
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

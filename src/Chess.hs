module Chess where

import           Data.List    (maximumBy, minimumBy)
import           Data.Ord     (comparing)

import           AI
import           Models.Board
import           Models.Error (WithError (..))
import           Models.Game
import           Models.Move
import           Models.Piece
import           Moves        (PieceMatch (..), findAllMoves,
                               findAllPiecesForColor, isGameOver, isInCheckMate,
                               movePiece)


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


withDelta :: Game -> Int
withDelta game = case turn game of
    White -> 1
    Black -> -1

scoreGame :: Game -> Score
scoreGame game = case turn game of
    White -> case isInCheckMate White game of
        True  -> Score (10000) False
        False -> Score (-1000) False
    Black -> case isInCheckMate White game of
        True  -> Score (-10000) False
        False -> Score (1000) False




scoreGame2 :: Game -> Score
scoreGame2 game = case turn game of
    White -> Score (scoreBoard game) False
    Black -> Score (-scoreBoard game) False
    where
        scoreBoard :: Game -> Int
        scoreBoard game = sum (map pieceValue whitePieces) - sum (map pieceValue blackPieces)
            where
                whitePieces = findAllPiecesForColor White (board game)
                blackPieces = findAllPiecesForColor Black (board game)
                pieceValue :: PieceMatch -> Int
                pieceValue p =
                    case pieceType (piece p) of
                        Pawn   -> 1
                        Knight -> 3
                        Bishop -> 3
                        Rook   -> 5
                        Queen  -> 9
                        King   -> 0
    -- [materialScore, positionScore, mobilityScore]
{-
Score (materialScore board + positionScore board + mobilityScore board) False

materialScore :: Game -> Int
materialScore board = sum (map pieceValue whitePieces) - sum (map pieceValue blackPieces)
  where
    whitePieces = piecesOnBoard White board
    blackPieces = piecesOnBoard Black board
    pieceValue piece =
        case piece of
            Pawn   -> 1
            Knight -> 3
            Bishop -> 3.25
            Rook   -> 5
            Queen  -> 9
            King   -> 0


positionScore :: ChessBoard -> Int
positionScore board =
    centralControl White board - centralControl Black board
  where
    centralControl color b =
        let centralPieces = filter (`elem` [D4, D5, E4, E5]) (piecesOfType Pawn color b)
            centralKnights = filter (`elem` [D4, D5, E4, E5]) (piecesOfType Knight color b)
        in length centralPieces + 2 * length centralKnights


mobilityScore :: ChessBoard -> Int
mobilityScore board =
    legalMoves White board - legalMoves Black board
  where
    legalMoves color b = length (allLegalMoves color b)
    -}



getInitialArgs :: Game -> Bool -> AlphaBetaArgs Game
getInitialArgs game max = AlphaBetaArgs {
    game  = game,
    depth = 1000,
    alpha = Score (-1000000000000) False,
    beta  = Score 1000000000000 False,
    isMax = max
}

chessAlphaBeta :: AlphaBetaArgs Game -> Score
chessAlphaBeta = alphaBeta isGameOver scoreGame expandGame

miniMax :: Game -> Game
miniMax game = case turn game of
    White -> maximumBy (comparing $ (\g -> chessAlphaBeta miniArgs { game = g })) possibleGames
    Black -> minimumBy (comparing $ (\g -> chessAlphaBeta maxiArgs { game = g })) possibleGames
    where
        possibleGames = expandGame game
        miniArgs = getInitialArgs game False
        maxiArgs = getInitialArgs game True

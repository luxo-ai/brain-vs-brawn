module AISpec (spec) where

import           AI         (AlphaBetaArgs (..), Score (..), alphaBeta2)
import qualified TicTacToe

import           Data.List  (maximumBy, minimumBy)
import           Data.Ord   (comparing)
import           Test.Hspec


emptyTicTacToeBoard :: TicTacToe.TicTacToeBoard
emptyTicTacToeBoard = TicTacToe.TicTacToeBoard Nothing Nothing Nothing Nothing

initialTicTacToe :: TicTacToe.TicTacToe
initialTicTacToe = TicTacToe.TicTacToe emptyTicTacToeBoard TicTacToe.X

ticTacToeAlphaBeta = alphaBeta2 TicTacToe.isGameOver (\t -> Score (TicTacToe.scoreGame t) False) TicTacToe.possibleGames

args :: AlphaBetaArgs TicTacToe.TicTacToe
args = AlphaBetaArgs {
    game  = initialTicTacToe,
    depth = 1000,
    alpha = Score 0 False,
    beta  = Score 0 False,
    isMax = True
}

minimax :: TicTacToe.TicTacToe -> TicTacToe.Move
minimax t@(TicTacToe.TicTacToe board turn) = case turn of
    TicTacToe.X -> maximumBy (comparing $ (\move -> ticTacToeAlphaBeta args { game = (TicTacToe.placeMove move t), isMax = False })) possibleMoves
    TicTacToe.O -> minimumBy (comparing $ (\move -> ticTacToeAlphaBeta args { game = (TicTacToe.placeMove move t), isMax = True })) possibleMoves
    where
        possibleMoves = TicTacToe.possibleMoves t

spec :: Spec
spec = do
    describe "alphaBeta" $ do
        describe "when given an empty list" $ do
            it "should return an empty list" $
                [1] `shouldBe` [1]


run :: IO ()
run = run1 initialTicTacToe
    where
        run1 :: TicTacToe.TicTacToe -> IO ()
        run1 game = do
            putStrLn $ show $ TicTacToe.board game
            run1 (TicTacToe.placeMove (minimax game) game)



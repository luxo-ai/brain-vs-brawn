module Models.GameSpec (spec) where

import           Models.Game   (Game (..), getPlayerForColor, getTurnPlayer,
                                incCurrentPlayerScore, toggleTurn)

import           Models.Board  (Board)
import           Models.Piece  (PieceColor (..))
import           Models.Player (Player (..))

import           Test.Hspec

testBoard :: Board
testBoard = []

testGame :: Game
testGame = Game {
    player1 = Player "sponge" Black 0,
    player2 = Player "bob" White 0,
    turn    = Black,
    board   = testBoard,
    prev    = Nothing
}

spec :: Spec
spec = do
    describe "getTurnPlayer" $ do
        describe "when given a game" $ do
            it "should return the player whose turn it is" $
                getTurnPlayer testGame `shouldBe` (player1 testGame)

    describe "getPlayerForColor" $ do
        describe "when the color is white" $ do
            it "should return the player with the given color" $
                getPlayerForColor White testGame `shouldBe` (player2 testGame)

        describe "when the color is black" $ do
            it "should return the player with the given color" $
                getPlayerForColor Black testGame `shouldBe` (player1 testGame)

    describe "toggleTurn" $ do
        describe "when given a game" $ do
            it "should return a game with the turn toggled" $
                toggleTurn testGame `shouldBe` testGame { turn = White }

    describe "incCurrentPlayerScore" $ do
        describe "when given a game" $ do
            it "should return a game with the current player's score incremented" $
                incCurrentPlayerScore testGame `shouldBe` testGame { player1 = (player1 testGame) { score = 1 } }

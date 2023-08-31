module Models.PieceSpec (spec) where

import           Models.Piece (Piece (..), PieceColor (..), PieceKind (..),
                               isSameColor, toggleColor)

import           Test.Hspec

spec :: Spec
spec = do
    describe "toggleColor" $ do
        describe "when given a white piece" $ do
            it "should return a black piece" $
                toggleColor White `shouldBe` Black

        describe "when given a black piece" $ do
            it "should return a white piece" $
                toggleColor Black `shouldBe` White

    describe "isSameColor" $ do
        describe "when given two pieces of the same color" $ do
            it "should return true" $
                isSameColor (Piece White King) (Piece White Rook) `shouldBe` True

        describe "when given two pieces of different colors" $ do
            it "should return false" $
                isSameColor (Piece White King) (Piece Black Rook) `shouldBe` False

        describe "when given two pieces of the same color" $ do
            it "should return true" $
                isSameColor (Piece Black King) (Piece Black Rook) `shouldBe` True

        describe "when given two pieces of different colors" $ do
            it "should return false" $
                isSameColor (Piece Black King) (Piece White Rook) `shouldBe` False

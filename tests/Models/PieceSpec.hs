module Models.PieceSpec (spec) where

import           Models.Piece (Piece (..), PieceColor (..), PieceKind (..),
                               toggleColor)

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

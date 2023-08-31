module Models.PlayerSpec (spec) where

import           Models.Piece  (PieceColor (..))
import           Models.Player (Player (..))

import           Test.Hspec

spec :: Spec
spec = do
    describe "Player" $ do
        describe "show white player" $ do
            it "a white player representation" $
                show (Player "sponge" White 0) `shouldBe` "sponge [White - 0] "

        describe "show black player" $ do
            it "a black player representation" $
                show (Player "bob" Black 0) `shouldBe` "bob [Black - 0] "


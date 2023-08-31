module Models.ErrorSpec (spec) where

import           Models.Error (ErrorType (..), WithError, unpackWithErrorsList)

import           Test.Hspec

spec :: Spec
spec = do
    describe "unpackWithErrors" $ do
        describe "when given an empty list" $ do
            it "should return an empty list" $
                unpackWithErrorsList [] `shouldBe` (Right [] :: WithError [Int])

        describe "when given a list without errors" $ do
            it "should return the list" $
                unpackWithErrorsList [Right 1, Right 2] `shouldBe` (Right [1, 2] :: WithError [Int])

        describe "when given a list with an error" $ do
            it "should return the first error" $
                unpackWithErrorsList [Left OutOfBounds, Left Occupied] `shouldBe` (Left OutOfBounds :: WithError [Int])

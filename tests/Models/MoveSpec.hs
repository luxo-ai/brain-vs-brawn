module Models.MoveSpec (spec) where

import           Models.Board (Board (..), BoardCell, Cell (..))
import           Models.Error (ErrorType (..), WithError (..))
import           Models.Move  (getCellAt, setCellAt)
import           Models.Piece (Piece (..), PieceColor (..), PieceKind (..))
import           Models.Posn  (Posn (..))

import           Test.Hspec

board3x3 :: Board
board3x3 = [
            [With (Piece White King), Empty, Empty],
            [Empty, Empty, Empty],
            [Empty, With (Piece Black King), Empty]
            ]

whitePawn :: Piece
whitePawn = Piece White Pawn

board3x3WithWhitePawn :: Board
board3x3WithWhitePawn = [
            [With whitePawn, Empty, Empty],
            [Empty, Empty, Empty],
            [Empty, With (Piece Black King), Empty]
            ]

spec :: Spec
spec = do
    describe "getCellAt" $ do
        describe "when given a valid position" $ do
            it "should return the cell at that position" $
                getCellAt (Posn 0 0) board3x3 `shouldBe` (Right (With (Piece White King)) :: WithError BoardCell)

        describe "when given an invalid position" $ do
            it "should return an error" $
                getCellAt (Posn 4 0) board3x3 `shouldBe` (Left OutOfBounds :: WithError BoardCell)

    describe "setCellAt" $ do
        describe "when given a valid position" $ do
            it "should return the board with the cell at that position set" $
                setCellAt (With whitePawn) (Posn 0 0) board3x3 `shouldBe` (Right board3x3WithWhitePawn :: WithError Board)

        describe "when given an invalid position" $ do
            it "should return an error" $
                setCellAt (With whitePawn) (Posn 5 2) board3x3 `shouldBe` (Left OutOfBounds :: WithError Board)
              --  setCellAt :: BoardCell -> Posn -> Board -> WithError Board

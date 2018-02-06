module GameBoardSpec where

import Test.Hspec
import GameBoard
import Orientation
import Pos
import Vessel

main :: IO ()
main = hspec $ do
  describe "GameBoard" $ do
    it "reports game is over when no vessels remain afloat" $ do
      let vs = [ bldDestroyer Horizontal (8,2) ]
      let ms = fromList [(1,2), (4,3)]
      let gb = GameBoard (0,0) (9,9) vs ms
      gameOver gb `shouldBe` False

    it "reports game is not over whilst some vessels remain afloat" $ do
      let v  = addHit (9,2) $ addHit (8,2) $ bldDestroyer Horizontal (8,2)
      let vs = [v]
      let ms = fromList [(1,2), (4,3)]
      let gb = GameBoard (0,0) (9,9) vs ms
      gameOver gb `shouldBe` True

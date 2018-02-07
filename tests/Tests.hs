module VesselSpec where

import Test.Hspec
import Orientation
import Vessel

main :: IO ()
main = hspec $ do
  describe "Vessel" $ do
    it "correctly identifies when a vessel is sunk" $ do
      let v1 = bldDestroyer Horizontal (8,2)
      let vs = [v1]
      let ms = fromList [(1,2), (4,3)]
      let gb = GameBoard (0,0) (9,9) vs ms
      gameOver gb `shouldBe` False

    it "reports game is not over whilst some vessels remain afloat" $ do
      let v   = bldDestroyer Horizontal (8,2)
      let v'  = addHit (8,2) v
      let v'' = addHit (9,2) v'
      let vs  = [v'']
      let ms  = fromList [(1,2), (4,3)]
      let gb  = GameBoard (0,0) (9,9) vs ms
      gameOver gb `shouldBe` True

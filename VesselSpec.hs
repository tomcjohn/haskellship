module VesselSpec where

import Test.Hspec
import Orientation
import Pos
import Vessel

main :: IO ()
main = hspec $ do
  describe "Vessel" $ do
    it "identifies when a single vessel is sunk" $ do
      let v = Vessel Destroyer Horizontal (fromList [(8,2),(9,2)]) empty
      isSunk v `shouldBe` False
      let v' = Vessel Destroyer Horizontal (fromList [(8,2),(9,2)]) (fromList [(8,2)])
      isSunk v' `shouldBe` False
      let v'' = Vessel Destroyer Horizontal (fromList [(8,2),(9,2)]) (fromList [(8,2),(9,2)])
      isSunk v'' `shouldBe` True

    it "identifies when all vessels are sunk" $ do
      let vs = [ Vessel Destroyer Horizontal (fromList [(8,2),(9,2)]) empty
               , Vessel Submarine Horizontal (fromList [(1,1),(2,1),(3,1)]) empty
               ]
      allSunk vs `shouldBe` False
      let vs' = [ Vessel Destroyer Horizontal (fromList [(8,2),(9,2)]) (fromList [(8,2),(9,2)])
               , Vessel Submarine Horizontal (fromList [(1,1),(2,1),(3,1)]) (fromList [(1,1),(2,1),(3,1)])
               ]
      allSunk vs' `shouldBe` True

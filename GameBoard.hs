module GameBoard where

import qualified Data.Set as Set
import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel] deriving Show

boardVessels :: GameBoard -> [Vessel]
boardVessels (GameBoard _ _ vs) = vs

takeShot :: GameBoard -> Position -> GameBoard
takeShot (GameBoard p1 p2 vs) shot = (GameBoard p1 p2 (shootVessels shot vs))

gameOver :: GameBoard -> Bool
gameOver (GameBoard _ _ vs) = allSunk vs
  where allSunk [] = True
        allSunk (h:t) = isSunk h && allSunk t

shootVessels :: Position -> [Vessel] -> [Vessel]
shootVessels _ [] = []
shootVessels shot (v:vs) = shootVessel shot v : shootVessels shot vs

shootVessel :: Position -> Vessel -> Vessel
shootVessel p (Carrier o ps hs) = if elem p ps then (Carrier o ps (Set.insert p hs)) else (Carrier o ps hs)
shootVessel p (Battleship o ps hs) = if elem p ps then (Battleship o ps (Set.insert p hs)) else (Battleship o ps hs)
shootVessel p (Cruiser o ps hs) = if elem p ps then (Cruiser o ps (Set.insert p hs)) else (Cruiser o ps hs)
shootVessel p (Submarine o ps hs) = if elem p ps then (Submarine o ps (Set.insert p hs)) else (Submarine o ps hs)
shootVessel p (Destroyer o ps hs) = if elem p ps then (Destroyer o ps (Set.insert p hs)) else (Destroyer o ps hs)

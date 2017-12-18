module Vessel where

import Orientation
import Pos

data Vessel = Carrier Orientation [Pos] |
              Battleship Orientation [Pos] |
              Cruiser Orientation [Pos] |
              Submarine Orientation [Pos] |
              Destroyer Orientation [Pos] deriving Show

buildCarrier :: Orientation -> Pos -> Vessel
buildCarrier o p = Carrier o (listPositions o p 5)
buildBattleship :: Orientation -> Pos -> Vessel
buildBattleship o p = Battleship o (listPositions o p 4)
buildCruiser :: Orientation -> Pos -> Vessel
buildCruiser o p = Cruiser o (listPositions o p 3)
buildSubmarine :: Orientation -> Pos -> Vessel
buildSubmarine o p = Submarine o (listPositions o p 3)
buildDestroyer :: Orientation -> Pos -> Vessel
buildDestroyer o p = Destroyer o (listPositions o p 2)

listPositions :: Orientation -> Pos -> Int -> [Pos]
listPositions o p l = if l > 0 then (p : (listPositions o (nextPos o p) (l-1))) else []

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

isSunk :: Vessel -> [Pos] -> Bool
isSunk (Carrier _ ps) hs = contains ps hs
isSunk (Battleship _ ps) hs = contains ps hs
isSunk (Cruiser _ ps) hs = contains ps hs
isSunk (Submarine _ ps) hs = contains ps hs
isSunk (Destroyer _ ps) hs = contains ps hs

contains :: [Pos] -> [Pos] -> Bool
contains [] _ = True
contains (p:ps) hs = elem p hs && contains ps hs

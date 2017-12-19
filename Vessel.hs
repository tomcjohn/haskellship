module Vessel where

import Orientation
import Pos

data Vessel = Carrier Orientation [Pos] |
              Battleship Orientation [Pos] |
              Cruiser Orientation [Pos] |
              Submarine Orientation [Pos] |
              Destroyer Orientation [Pos] deriving Show

bldCarrier :: Orientation -> Pos -> Vessel
bldCarrier o p = Carrier o (listPositions o p 5)
bldBattleship :: Orientation -> Pos -> Vessel
bldBattleship o p = Battleship o (listPositions o p 4)
bldCruiser :: Orientation -> Pos -> Vessel
bldCruiser o p = Cruiser o (listPositions o p 3)
bldSubmarine :: Orientation -> Pos -> Vessel
bldSubmarine o p = Submarine o (listPositions o p 3)
bldDestroyer :: Orientation -> Pos -> Vessel
bldDestroyer o p = Destroyer o (listPositions o p 2)

listPositions :: Orientation -> Pos -> Int -> [Pos]
listPositions o p l = if l > 0 then (p : (listPositions o (nextPos o p) (l-1))) else []

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

didItHit :: [Vessel] -> Pos -> Bool
didItHit [] _ = False
didItHit (v:vs) shot = do
  if isHit v shot
    then True
    else didItHit vs shot

isHit :: Vessel -> Pos -> Bool
isHit (Carrier _ ps) shot = elem shot ps
isHit (Battleship _ ps) shot = elem shot ps
isHit (Cruiser _ ps) shot = elem shot ps
isHit (Submarine _ ps) shot = elem shot ps
isHit (Destroyer _ ps) shot = elem shot ps

allSunk :: [Vessel] -> [Pos] -> Bool
allSunk [] _ = True
allSunk (v:vs) hits = isSunk v hits && allSunk vs hits

isSunk :: Vessel -> [Pos] -> Bool
isSunk (Carrier _ ps) hs = contains ps hs
isSunk (Battleship _ ps) hs = contains ps hs
isSunk (Cruiser _ ps) hs = contains ps hs
isSunk (Submarine _ ps) hs = contains ps hs
isSunk (Destroyer _ ps) hs = contains ps hs

contains :: [Pos] -> [Pos] -> Bool
contains [] _ = True
contains (p:ps) hs = elem p hs && contains ps hs

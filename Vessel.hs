module Vessel where

import Orientation
import Pos

-- A vessel type, constructed from an orientation, a list of the positions it inhabits
-- and a list of the positions where shots have hit.
data Vessel = Carrier Orientation [Pos] [Pos] |
              Battleship Orientation [Pos] [Pos] |
              Cruiser Orientation [Pos] [Pos] |
              Submarine Orientation [Pos] [Pos] |
              Destroyer Orientation [Pos] [Pos] deriving Show

class HasPositions a where
  positions :: a -> [Pos]

instance HasPositions Vessel where
  positions (Carrier _ ps _) = ps
  positions (Battleship _ ps _) = ps
  positions (Cruiser _ ps _) = ps
  positions (Submarine _ ps _) = ps
  positions (Destroyer _ ps _) = ps

bldCarrier :: Orientation -> Pos -> Vessel
bldCarrier o p = Carrier o (listPositions o p 5) []
bldBattleship :: Orientation -> Pos -> Vessel
bldBattleship o p = Battleship o (listPositions o p 4) []
bldCruiser :: Orientation -> Pos -> Vessel
bldCruiser o p = Cruiser o (listPositions o p 3) []
bldSubmarine :: Orientation -> Pos -> Vessel
bldSubmarine o p = Submarine o (listPositions o p 3) []
bldDestroyer :: Orientation -> Pos -> Vessel
bldDestroyer o p = Destroyer o (listPositions o p 2) []

listPositions :: Orientation -> Pos -> Int -> [Pos]
listPositions o p l = if l > 0 then (p : (listPositions o (nextPos o p) (l-1))) else []

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

addHit :: Pos -> Vessel -> Vessel
addHit shot (Carrier o ps hs) = (Carrier o ps (shot:hs))
addHit shot (Battleship o ps hs) = (Battleship o ps (shot:hs))
addHit shot (Cruiser o ps hs) = (Cruiser o ps (shot:hs))
addHit shot (Submarine o ps hs) = (Submarine o ps (shot:hs))
addHit shot (Destroyer o ps hs) = (Destroyer o ps (shot:hs))

isHit :: Pos -> Vessel -> Bool
isHit shot (Carrier _ ps _) = elem shot ps
isHit shot (Battleship _ ps _) = elem shot ps
isHit shot (Cruiser _ ps _) = elem shot ps
isHit shot (Submarine _ ps _) = elem shot ps
isHit shot (Destroyer _ ps _) = elem shot ps

vesselHits :: Vessel -> [Pos]
vesselHits (Carrier _ _ hs) = hs
vesselHits (Battleship _ _ hs) = hs
vesselHits (Cruiser _ _ hs) = hs
vesselHits (Submarine _ _ hs) = hs
vesselHits (Destroyer _ _ hs) = hs

allSunk :: [Vessel] -> Bool
allSunk [] = True
allSunk (v:vs) = isSunk v && allSunk vs

isSunk :: Vessel -> Bool
isSunk (Carrier _ ps hs) = contains ps hs
isSunk (Battleship _ ps hs) = contains ps hs
isSunk (Cruiser _ ps hs) = contains ps hs
isSunk (Submarine _ ps hs) = contains ps hs
isSunk (Destroyer _ ps hs) = contains ps hs

contains :: [Pos] -> [Pos] -> Bool
contains [] _ = True
contains (p:ps) hs = elem p hs && contains ps hs

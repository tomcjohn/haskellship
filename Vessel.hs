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

-- Replace the Pos lists above with Sets instead and type alias a PosSet in Pos.hs

-- pull vessel type out of Vessel above

-- replace positions function with record field
positions :: Vessel -> [Pos]
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

vesselType :: Vessel -> String
vesselType (Carrier _ _ _) = "carrier"
vesselType (Battleship _ _ _) = "battleship"
vesselType (Cruiser _ _ _) = "cruiser"
vesselType (Submarine _ _ _) = "submarine"
vesselType (Destroyer _ _ _) = "destroyer"

allSunk :: [Vessel] -> Bool
allSunk vs = all isSunk vs

-- when using Sets this should just become set equality
isSunk :: Vessel -> Bool
isSunk (Carrier _ ps hs) = contains ps hs
isSunk (Battleship _ ps hs) = contains ps hs
isSunk (Cruiser _ ps hs) = contains ps hs
isSunk (Submarine _ ps hs) = contains ps hs
isSunk (Destroyer _ ps hs) = contains ps hs

contains :: [Pos] -> [Pos] -> Bool
contains ps hs = all (flip elem hs) ps

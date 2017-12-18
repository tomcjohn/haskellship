module Vessel where

import Orientation
import Pos

data Vessel = Carrier Orientation [Pos] [Pos] |
              Battleship Orientation [Pos] [Pos] |
              Cruiser Orientation [Pos] [Pos] |
              Submarine Orientation [Pos] [Pos] |
              Destroyer Orientation [Pos] [Pos] deriving Show

buildCarrier :: Orientation -> Pos -> Vessel
buildCarrier o p = Carrier o (listPositions o p 5) []
buildBattleship :: Orientation -> Pos -> Vessel
buildBattleship o p = Battleship o (listPositions o p 4) []
buildCruiser :: Orientation -> Pos -> Vessel
buildCruiser o p = Cruiser o (listPositions o p 3) []
buildSubmarine :: Orientation -> Pos -> Vessel
buildSubmarine o p = Submarine o (listPositions o p 3) []
buildDestroyer :: Orientation -> Pos -> Vessel
buildDestroyer o p = Destroyer o (listPositions o p 2) []

listPositions :: Orientation -> Pos -> Int -> [Pos]
listPositions o p l = if l > 0 then (p : (listPositions o (nextPos o p) (l-1))) else []

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

vesselContains :: Pos -> Vessel -> Bool
vesselContains p (Carrier _ ps _) = elem p ps
vesselContains p (Battleship _ ps _) = elem p ps
vesselContains p (Cruiser _ ps _) = elem p ps
vesselContains p (Submarine _ ps _) = elem p ps
vesselContains p (Destroyer _ ps _) = elem p ps

addHit :: Pos -> Vessel -> Vessel
addHit p (Carrier o ps hs) = Carrier o ps (p : hs)
addHit p (Battleship o ps hs) = Battleship o ps (p : hs)
addHit p (Cruiser o ps hs) = Cruiser o ps (p : hs)
addHit p (Submarine o ps hs) = Submarine o ps (p : hs)
addHit p (Destroyer o ps hs) = Destroyer o ps (p : hs)

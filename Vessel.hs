module Vessel where

import qualified Data.Set as Set

import Orientation
import Pos

data VesselType = Carrier | Battleship | Cruiser | Submarine | Destroyer deriving Show

-- A vessel is constructed from a vessel type, an orientation, a list of the positions
-- it inhabits and a list of the positions where it has been hit by shots.
data Vessel = Vessel {
  vesselType::VesselType,
  orientation::Orientation,
  positions::PosSet,
  hits::PosSet
} deriving Show

bldCarrier :: Orientation -> Pos -> Vessel
bldCarrier o p = Vessel Carrier o (listPositions o p 5) Set.empty
bldBattleship :: Orientation -> Pos -> Vessel
bldBattleship o p = Vessel Battleship o (listPositions o p 4) Set.empty
bldCruiser :: Orientation -> Pos -> Vessel
bldCruiser o p = Vessel Cruiser o (listPositions o p 3) Set.empty
bldSubmarine :: Orientation -> Pos -> Vessel
bldSubmarine o p = Vessel Submarine o (listPositions o p 3) Set.empty
bldDestroyer :: Orientation -> Pos -> Vessel
bldDestroyer o p = Vessel Destroyer o (listPositions o p 2) Set.empty

listPositions :: Orientation -> Pos -> Int -> PosSet
listPositions o p l = if l > 0 then Set.insert p (listPositions o (nextPos o p) (l-1)) else Set.empty

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

addHit :: Pos -> Vessel -> Vessel
addHit s v = v {hits=Set.insert s (hits v)}

isHit :: Pos -> Vessel -> Bool
isHit s v = elem s (positions v)

listHits :: [Vessel] -> PosSet
listHits [] = Set.empty
listHits (v:vs) = Set.union (hits v) (listHits vs)

allSunk :: [Vessel] -> Bool
allSunk vs = all isSunk vs

isSunk :: Vessel -> Bool
isSunk v = (positions v) == (hits v)

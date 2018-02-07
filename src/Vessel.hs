module Vessel where

import Orientation
import Pos

data VesselType = Carrier | Battleship | Cruiser | Submarine | Destroyer | NoVessel deriving Show

data PositionedVessel = PositionedVessel
  { vesselType::VesselType
  , orientation::Orientation
  , positions::PosSet
  } deriving Show

bldCarrier :: Orientation -> Pos -> PositionedVessel
bldCarrier o p = PositionedVessel Carrier o (listPositions o p 5)
bldBattleship :: Orientation -> Pos -> PositionedVessel
bldBattleship o p = PositionedVessel Battleship o (listPositions o p 4)
bldCruiser :: Orientation -> Pos -> PositionedVessel
bldCruiser o p = PositionedVessel Cruiser o (listPositions o p 3)
bldSubmarine :: Orientation -> Pos -> PositionedVessel
bldSubmarine o p = PositionedVessel Submarine o (listPositions o p 3)
bldDestroyer :: Orientation -> Pos -> PositionedVessel
bldDestroyer o p = PositionedVessel Destroyer o (listPositions o p 2)

listPositions :: Orientation -> Pos -> Int -> PosSet
listPositions o p l = if l > 0 then insert p (listPositions o (nextPos o p) (l-1)) else empty

nextPos :: Orientation -> Pos -> Pos
nextPos Horizontal (x,y) = ((x+1), y)
nextPos Vertical   (x,y) = (x, (y+1))

module Vessel where

import Orientation
import Position

data Vessel = Carrier Orientation Position |
              Battleship Orientation Position |
              Cruiser Orientation Position |
              Submarine Orientation Position |
              Destroyer Orientation Position

vesselPositions :: Vessel -> [Position]
vesselPositions (Carrier o p) = listPositions o p 5
vesselPositions (Battleship o p) = listPositions o p 4
vesselPositions (Cruiser o p) = listPositions o p 3
vesselPositions (Submarine o p) = listPositions o p 3
vesselPositions (Destroyer o p) = listPositions o p 2

listPositions :: Orientation -> Position -> Int -> [Position]
listPositions o p l = if l > 0 then p : (listPositions o (nextPos o p) (l-1)) else []

nextPos :: Orientation -> Position -> Position
nextPos Horizontal (Position x y) = Position (x+1) y
nextPos Vertical   (Position x y) = Position x (y+1)

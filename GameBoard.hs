module GameBoard where

import Position
import Orientation
import Vessel

data GameBoard = GameBoard Position Position [Vessel] [[Position]] deriving Show

-- assumes bottom left is 0,0
makeBoard :: Position -> [Vessel] -> GameBoard
makeBoard topRight vessels = GameBoard (Position 0 0) topRight vessels (allVesselPositions vessels)

allVesselPositions :: [Vessel] -> [[Position]]
allVesselPositions [] = []
allVesselPositions (v:vs) = [(generateVesselPositions v)] ++ (allVesselPositions vs)

generateVesselPositions :: Vessel -> [Position]
generateVesselPositions v = vesselPositions v (vesselLength v)

vesselPositions :: Vessel -> Int -> [Position]
vesselPositions (Carrier o p) l = allThePositions o p l
vesselPositions (Battleship o p) l = allThePositions o p l
vesselPositions (Cruiser o p) l = allThePositions o p l
vesselPositions (Submarine o p) l = allThePositions o p l
vesselPositions (Destroyer o p) l = allThePositions o p l

allThePositions :: Orientation -> Position -> Int -> [Position]
allThePositions o p l = doAllThePositions o p l []
  where doAllThePositions o pos length acc = if l == 0 then acc else doAllThePositions Vertical (advancePosition o pos) (length-1) (pos : acc)

advancePosition :: Orientation -> Position -> Position
advancePosition Vertical (Position x y) = (Position x (y+1))
advancePosition Horizontal (Position x y) = (Position (x+1) y)

vesselLength :: Vessel -> Int
vesselLength (Carrier _ _) = 5
vesselLength (Battleship _ _) = 4
vesselLength (Cruiser _ _) = 3
vesselLength (Submarine _ _) = 3
vesselLength (Destroyer _ _) = 2

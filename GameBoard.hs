module GameBoard where

import Data.Set (Set)
import qualified Data.Set as Set
--import Debug.Trace

import Position
import Orientation
import Vessel

type HitSet = Set Position

data PositionedVessel = PositionedVessel Vessel [Position] HitSet deriving Show
data GameBoard = GameBoard Position Position [PositionedVessel] deriving Show

-- assumes bottom left is 0,0
makeBoard :: Position -> [Vessel] -> GameBoard
makeBoard topRight vessels = GameBoard (Position 0 0) topRight (allVesselPositions vessels)

allVesselPositions :: [Vessel] -> [PositionedVessel]
allVesselPositions [] = []
allVesselPositions (v:vs) = (PositionedVessel v (vesselPositions v (vesselLength v)) Set.empty) : allVesselPositions vs

vesselPositions :: Vessel -> Int -> [Position]
vesselPositions (Carrier o p) l = allThePositions o p l
vesselPositions (Battleship o p) l = allThePositions o p l
vesselPositions (Cruiser o p) l = allThePositions o p l
vesselPositions (Submarine o p) l = allThePositions o p l
vesselPositions (Destroyer o p) l = allThePositions o p l

allThePositions :: Orientation -> Position -> Int -> [Position]
allThePositions orient startPos len = doIt orient startPos len []
  where doIt _ _ 0 acc = acc
        doIt o p l acc = doIt o (advancePosition o p) (l-1) (acc ++ [p])

advancePosition :: Orientation -> Position -> Position
advancePosition o p = posAdd o p 1

vesselLength :: Vessel -> Int
vesselLength (Carrier _ _) = 5
vesselLength (Battleship _ _) = 4
vesselLength (Cruiser _ _) = 3
vesselLength (Submarine _ _) = 3
vesselLength (Destroyer _ _) = 2

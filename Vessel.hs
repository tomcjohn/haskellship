module Vessel where

import Data.Set (Set)
import qualified Data.Set as Set
import Orientation
import Position

type PosSet = Set Position
data Vessel = Carrier Orientation PosSet PosSet |
              Battleship Orientation PosSet PosSet |
              Cruiser Orientation PosSet PosSet |
              Submarine Orientation PosSet PosSet |
              Destroyer Orientation PosSet PosSet deriving Show

class HasSunk a where
  hasSunk :: a -> Bool

instance HasSunk Vessel where
  hasSunk (Carrier _ ps hs) = setEquals ps hs
  hasSunk (Battleship _ ps hs) = setEquals ps hs
  hasSunk (Cruiser _ ps hs) = setEquals ps hs
  hasSunk (Submarine _ ps hs) = setEquals ps hs
  hasSunk (Destroyer _ ps hs) = setEquals ps hs

setEquals :: PosSet -> PosSet -> Bool
setEquals ps hs = Set.null (Set.difference ps hs)

buildCarrier :: Orientation -> Position -> Vessel
buildCarrier o p = Carrier o (listPositions o p 5) Set.empty
buildBattleship :: Orientation -> Position -> Vessel
buildBattleship o p = Battleship o (listPositions o p 4) Set.empty
buildCruiser :: Orientation -> Position -> Vessel
buildCruiser o p = Cruiser o (listPositions o p 3) Set.empty
buildSubmarine :: Orientation -> Position -> Vessel
buildSubmarine o p = Submarine o (listPositions o p 3) Set.empty
buildDestroyer :: Orientation -> Position -> Vessel
buildDestroyer o p = Destroyer o (listPositions o p 2) Set.empty

listPositions :: Orientation -> Position -> Int -> PosSet
listPositions o p l = if l > 0 then (Set.insert p (listPositions o (nextPos o p) (l-1))) else Set.empty

nextPos :: Orientation -> Position -> Position
nextPos Horizontal (Position x y) = Position (x+1) y
nextPos Vertical   (Position x y) = Position x (y+1)

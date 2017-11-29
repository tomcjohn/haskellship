module Vessel where

import Orientation
import Position

data Vessel = Carrier Orientation Position |
              Battleship Orientation Position |
              Cruiser Orientation Position |
              Submarine Orientation Position |
              Destroyer Orientation Position deriving Show

class WasHit a where
  wasHit :: a -> Position -> Bool

instance HasPosition Vessel where
  pos (Carrier _ p) = p
  pos (Battleship _ p) = p
  pos (Cruiser _ p) = p
  pos (Submarine _ p) = p
  pos (Destroyer _ p) = p

instance WasHit Vessel where
  wasHit (Carrier o p) shotPos = didItHit o p 5 shotPos
  wasHit (Battleship o p) shotPos = didItHit o p 4 shotPos
  wasHit (Cruiser o p) shotPos = didItHit o p 4 shotPos
  wasHit (Submarine o p) shotPos = didItHit o p 3 shotPos
  wasHit (Destroyer o p) shotPos = didItHit o p 3 shotPos

didItHit :: Orientation -> Position -> Int -> Position -> Bool
didItHit o p l shot = elem shot (allPositions o p l)

allPositions :: Orientation -> Position -> Int -> [Position]
allPositions orient position len = doIt orient position len []
  where doIt o p l acc = if l > 0 then (doIt o (posAdd o p 1) (l-1) (p:acc)) else p:acc

posAdd :: Orientation -> Position -> Int -> Position
posAdd Vertical (Position x y) l = Position x (y+l)
posAdd Horizontal (Position x y) l = Position (x+l) y

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
didItHit Vertical (Position x y) l (Position shotX shotY) = undefined
didItHit Horizontal (Position x y) l (Position shotX shotY) = undefined

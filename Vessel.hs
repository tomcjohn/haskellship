module Vessel where

import Position

data Vessel = Carrier Position | Battleship Position | Cruiser Position | Submarine Position | Destroyer Position

instance HasPosition Vessel where
  pos (Carrier p) = p
  pos (Battleship p) = p
  pos (Cruiser p) = p
  pos (Submarine p) = p
  pos (Destroyer p) = p

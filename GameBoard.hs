module GameBoard where

import Position
import Vessel

data Dimensions = Dimensions Position Position

data GameBoard = GameBoard Dimensions [Vessel]

module GameBoard where

import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel]

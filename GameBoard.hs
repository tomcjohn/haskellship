module GameBoard where

import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel] deriving Show

takeShot :: Position -> GameBoard -> GameBoard
takeShot shot (GameBoard p1 p2 vessels) = (GameBoard p1 p2 (shootVessels shot vessels))
  where shootVessels _ [] = []
        shootVessels p (v:vs) = if vesselContains p v then addHit p v : vs else v : shootVessels p vs

gameOver :: GameBoard -> Bool
gameOver (GameBoard _ _ vessels) = allSunk vessels
  where allSunk [] = True
        allSunk (v:vs) = isSunk v && allSunk vs

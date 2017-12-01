module GameBoard where

import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel] deriving Show

takeShot :: GameBoard -> Position -> GameBoard
takeShot (GameBoard p1 p2 vessels) shot = (GameBoard p1 p2 (shootVessels shot vessels))
  where shootVessels _ [] = []
        shootVessels p (v:vs) = if vesselContains p v then addHit p v : vs else v : shootVessels p vs

gameOver :: GameBoard -> Bool
gameOver (GameBoard _ _ vessels) = allSunk vessels
  where allSunk [] = True
        allSunk (v:vs) = isSunk v && allSunk vs

allBoardPositions :: GameBoard -> IO ()
allBoardPositions (GameBoard _ _ vessels) = doIt vessels
  where doIt [] = putStrLn ""
        doIt (v:vs) = do
          print v
          doIt vs

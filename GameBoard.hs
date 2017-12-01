module GameBoard where

import Control.Monad
import Debug.Trace
import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel] deriving Show

takeShot :: GameBoard -> Position -> GameBoard
takeShot (GameBoard p1 p2 vessels) shot =
  if not (onBoard p1 p2 shot) then
    trace ("Off board: " ++ (show shot)) GameBoard p1 p2 vessels
  else
    trace ("Shot: " ++ (show shot)) GameBoard p1 p2 (shoot shot vessels)
  where shoot _ [] = []
        shoot p (v:vs) =
          if vesselContains p v then
            addHit p v : vs
          else
            v : shoot p vs

onBoard :: Position -> Position -> Position -> Bool
onBoard (Position x1 y1) (Position x2 y2) (Position x y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> Bool
gameOver (GameBoard _ _ vessels) = allSunk vessels
  where allSunk [] = True
        allSunk (v:vs) = isSunk v && allSunk vs

allBoardPositions :: GameBoard -> IO ()
allBoardPositions (GameBoard _ _ vs) = forM_ vs print

module Main where

import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

import GameBoard
import Orientation
import Position
import Vessel

createBoard :: GameBoard
createBoard = do
  let ca = Carrier Vertical (Position 0 2)
  let b = Battleship Horizontal (Position 4 1)
  let cr = Cruiser Horizontal (Position 3 8)
  let s = Submarine Horizontal (Position 5 5)
  let d = Destroyer Vertical (Position 4 4)
  makeBoard (Position 9 9) [ca, b, cr, s, d]

-- TODO check shot is on the board
playerTurn :: GameBoard -> Position -> GameBoard
playerTurn (GameBoard bottomLeft topRight []) _ = GameBoard bottomLeft topRight []
playerTurn (GameBoard bottomLeft topRight pvs) shot = GameBoard bottomLeft topRight (takeTheShot pvs shot)

-- TODO not exhaustive on a positioned vessel's positions
takeTheShot :: [PositionedVessel] -> Position -> [PositionedVessel]
takeTheShot [] _ = []
takeTheShot ((PositionedVessel v [] hits):pvs) _ = ((PositionedVessel v [] hits):pvs)
takeTheShot ((PositionedVessel v positions hits):pvs) shot = (PositionedVessel v positions (calculateHit shot positions hits)) : (takeTheShot pvs shot)

calculateHit :: Position -> [Position] -> HitSet -> HitSet
calculateHit shot positions hits =
  if (elem shot positions)
  then trace ("HIT") (Set.insert shot hits)
  else trace ("MISS") hits

main :: IO ()
main = do
  let gb = createBoard
  print gb
  let newGb = playerTurn gb (Position 5 5)
  print newGb

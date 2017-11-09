module Main where

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

main :: IO ()
main = do
  putStrLn "Hello World!"
  let gb = createBoard
  print gb
  putStrLn "done"

module Main where

import GameBoard
import Orientation
import Position
import Vessel

createBoard :: GameBoard
createBoard = do
  let ca = Carrier Vertical (Position 1 3)
  let b = Battleship Vertical (Position 2 2)
  let cr = undefined
  let s = undefined
  let d = undefined
  GameBoard (Dimensions (Position 0 0) (Position 10 10)) [ca, b, cr, s, d]

main :: IO ()
main = do
  putStrLn "Hello World!"
  let gb = createBoard
  putStrLn "done"

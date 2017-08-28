module Main where

import GameBoard
import Position
import Vessel

createBoard :: GameBoard
createBoard = do
  let ca = Carrier (Position 1 3)
  let b = undefined
  let cr = undefined
  let s = undefined
  let d = undefined
  GameBoard (Dimensions (Position 0 0) (Position 10 10)) [ca, b, cr, s, d]

main :: IO ()
main = do
  putStrLn "Hello World!"
  let gb = createBoard
  putStrLn "done"

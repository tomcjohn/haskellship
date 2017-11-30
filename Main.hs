module Main where

import GameBoard
import Orientation
import Position
import Vessel

main :: IO ()
main = do
  putStrLn "Calculating board ..."
  let carrier = buildCarrier Vertical (Position 0 2)
  let battleship = buildBattleship Horizontal (Position 4 1)
  let cruiser = buildCruiser Horizontal (Position 3 8)
  let submarine = buildSubmarine Horizontal (Position 5 5)
  let destroyer = buildDestroyer Vertical (Position 4 4)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  let board = GameBoard (Position 0 0) (Position 9 9) vessels
  putStrLn "Game Over - You lose!"

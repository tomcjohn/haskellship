module Main where

import GameBoard
import Orientation
import Position
import Vessel

generateBoard :: GameBoard
generateBoard = do
  let carrier = buildCarrier Vertical (Position 0 2)
  let battleship = buildBattleship Horizontal (Position 4 1)
  let cruiser = buildCruiser Horizontal (Position 3 8)
  let submarine = buildSubmarine Horizontal (Position 5 5)
  let destroyer = buildDestroyer Vertical (Position 4 4)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  GameBoard (Position 0 0) (Position 9 9) vessels

runGame :: GameBoard -> [Position] -> GameBoard
runGame b [] = b
runGame board (p:ps) = do
  let newBoard = takeShot board p
  runGame newBoard ps

main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  let shots = [Position 0 2,Position 0 3,Position 0 4,Position 0 5,Position 0 6,
               Position 4 1,Position 5 1,Position 6 1,Position 7 1,
               Position 3 8,Position 4 8,Position 5 8,
               Position 5 5,Position 6 5,Position 7 5,
               Position 4 4,Position 4 5]
  let finalBoard = runGame board shots
  putStrLn "Game Over!"
  putStrLn ("All sunk = " ++ (show (gameOver finalBoard)))

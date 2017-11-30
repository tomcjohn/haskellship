module Main where

import GameBoard
import Orientation
import Position
import Vessel

main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  let newBoard = takeShot nextShot board
  print (gameOver newBoard)
  putStrLn "Game Over - You lose!"

generateBoard :: GameBoard
generateBoard = do
  let carrier = buildCarrier Vertical (Position 0 2)
  let battleship = buildBattleship Horizontal (Position 4 1)
  let cruiser = buildCruiser Horizontal (Position 3 8)
  let submarine = buildSubmarine Horizontal (Position 5 5)
  let destroyer = buildDestroyer Vertical (Position 4 4)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  GameBoard (Position 0 0) (Position 9 9) vessels

nextShot :: Position
nextShot = do
  -- TODO get input from user
  let nextX = 0
  let nextY = 2
  Position nextX nextY

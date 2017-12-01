module Main where

import qualified Data.List.Split as S
import GameBoard
import Orientation
import Position
import Vessel

-- TODO add random placement for vessels on the board
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

strToPos :: String -> Position
strToPos s = do
  let splitPos = S.splitOn "," s
  let x = read (splitPos!!0)
  let y = read (splitPos!!1)
  Position x y

-- TODO accept user input from keyboard
-- TODO provide a way of displaying the board at any point such that the user can decide where to shoot next
main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  let filename = "shots-test.in"
  content <- readFile filename
  let shots = map strToPos (lines content)
  let finalBoard = runGame board shots
  putStrLn "Game Over!"
  putStrLn ("All sunk = " ++ (show (gameOver finalBoard)))

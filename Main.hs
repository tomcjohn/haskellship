module Main where

import qualified Data.List.Split as S
import GameBoard
import Orientation
import Position
import Vessel

-- TODO add random placement for vessels on the board
generateBoard :: IO GameBoard
generateBoard = do
  let carrier = buildCarrier Vertical (Position 0 2)
  let battleship = buildBattleship Horizontal (Position 4 1)
  let cruiser = buildCruiser Horizontal (Position 3 8)
  let submarine = buildSubmarine Horizontal (Position 5 5)
  let destroyer = buildDestroyer Vertical (Position 4 4)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  pure (GameBoard (Position 0 0) (Position 9 9) vessels)

strToPos :: String -> Position
strToPos s = do
  let splitPos = S.splitOn "," s
  let x = read (splitPos!!0)
  let y = read (splitPos!!1)
  Position x y

runGame :: IO GameBoard -> IO ()
runGame ioBoard = do
  board <- ioBoard
  sunk <- gameOver board
  putStrLn ("All sunk = " ++ (show sunk))
  over <- gameOver board
  if over
    then putStrLn "Game Over!"
    else do
      print board
      putStr "Take a shot (x,y): "
      line <- getLine
      if null line
        then return ()
        else do
          let shot = strToPos line
          let newBoard = takeShot board shot
          runGame newBoard

boardy :: GameBoard -> IO GameBoard
boardy b = return b

-- TODO provide a way of displaying the board at any point such that the user can decide where to shoot next
main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  runGame board

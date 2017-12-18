module Main where

import qualified Data.List.Split as S
import GameBoard
import Orientation
import Pos
import Vessel

-- TODO add random placement for vessels on the board
generateBoard :: IO GameBoard
generateBoard = do
  let carrier = buildCarrier Vertical (0,2)
  let battleship = buildBattleship Horizontal (4,1)
  let cruiser = buildCruiser Horizontal (3,8)
  let submarine = buildSubmarine Horizontal (5,5)
  let destroyer = buildDestroyer Vertical (4,4)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  pure (GameBoard (0,0) (9,9) vessels [])

strToPos :: String -> Pos
strToPos s = do
  let splitPos = S.splitOn "," s
  let x = read (splitPos!!0)
  let y = read (splitPos!!1)
  (x,y)

runGame :: IO GameBoard -> IO ()
runGame ioBoard = do
  board <- ioBoard
  printBoard board
  over <- gameOver board
  if over
    then do
      putStrLn "Game Over"
      putStrLn "YOU WIN!!!"
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

main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  runGame board

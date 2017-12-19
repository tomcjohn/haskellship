module Main where

import qualified Data.List.Split as S
import System.Random
import GameBoard
import Orientation
import Pos
import Vessel

-- TODO add random placement for vessels on the board
generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  carrier <- (buildVessel buildCarrier bL tR)
  battleship <- (buildVessel buildBattleship bL tR)
  cruiser <- (buildVessel buildCruiser bL tR)
  submarine <- (buildVessel buildSubmarine bL tR)
  destroyer <- (buildVessel buildDestroyer bL tR)
  let vessels = [carrier, battleship, cruiser, submarine, destroyer]
  pure (GameBoard bL tR vessels [])

buildVessel :: (Orientation -> Pos -> Vessel) -> Pos -> Pos -> IO Vessel
buildVessel f bL tR = do
  o <- randomOrient
  p <- randomPos bL tR
  pure $ f o p

randomOrient :: IO Orientation
randomOrient = do
  pure Vertical

randomPos :: Pos -> Pos -> IO Pos
randomPos (x1,y1) (x2,y2) = do
  g <- getStdGen
  r1 <- randomNumber (x1,x2) g
  r2 <- randomNumber (y1,y2) (snd r1)
  pure (fst r1,fst r2)

randomNumber :: (Int, Int) -> StdGen -> IO (Int,StdGen)
randomNumber bounds g = do
  pure $ randomR bounds g

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

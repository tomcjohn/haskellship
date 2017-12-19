module Main where

import qualified Data.List.Split as S
import System.Random
import GameBoard
import Orientation
import Pos
import Vessel

-- TODO don't allow vessels to be positioned on top of each other
-- TODO don't allow vessels to be positioned off the board
generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  g <- getStdGen
  let vesselBuilders = [bldCarrier, bldBattleship, bldCruiser, bldSubmarine, bldDestroyer]
  vessels <- sequence $ buildVessels vesselBuilders g bL tR
  pure (GameBoard bL tR vessels [] [])

buildVessels :: [(Orientation -> Pos -> Vessel)] -> StdGen -> Pos -> Pos -> [IO Vessel]
buildVessels funcs gen bL tR = map (\f -> buildVessel f gen bL tR) funcs

buildVessel :: (Orientation -> Pos -> Vessel) -> StdGen -> Pos -> Pos -> IO Vessel
buildVessel f g bL tR = do
  o <- randomOrient
  p <- randomPos g bL tR
  pure $ f o p

randomOrient :: IO Orientation
randomOrient = do
  pure Vertical

randomPos :: StdGen -> Pos -> Pos -> IO Pos
randomPos g (x1,y1) (x2,y2) = do
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

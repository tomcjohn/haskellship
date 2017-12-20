module Main where

import qualified Data.List.Split as S
import System.Random
import GameBoard
import Orientation
import Pos
import Vessel

-- TODO don't allow vessels to be positioned on top of each other
generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldCarrier, bldBattleship, bldCruiser, bldSubmarine, bldDestroyer]
  vessels <- sequence $ buildVessels vesselBuilders bL tR
  pure (GameBoard bL tR vessels [] [])

buildVessels :: [(Orientation -> Pos -> Vessel)] -> Pos -> Pos -> [IO Vessel]
buildVessels funcs bL tR = map (\f -> buildVessel f bL tR) funcs

buildVessel :: (Orientation -> Pos -> Vessel) -> Pos -> Pos -> IO Vessel
buildVessel f bL tR = do
  o <- randomOrient
  p <- randomPos bL tR
  let vessel = f o p
  if vesselOffBoard tR vessel
    then do
      putStrLn ("Vessel off board: " ++ (show vessel))
      buildVessel f bL tR
    else pure vessel

randomOrient :: IO Orientation
randomOrient = do
  pure Vertical

randomPos :: Pos -> Pos -> IO Pos
randomPos (x1,y1) (x2,y2) = do
  x <- randomNumber (x1,x2)
  y <- randomNumber (y1,y2)
  pure (x,y)

randomNumber :: (Int, Int) -> IO Int
randomNumber bounds = do
  g <- getStdGen
  let r = randomR bounds g
  setStdGen (snd r)
  pure $ fst r

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

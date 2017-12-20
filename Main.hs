module Main where

import qualified Data.List as L
import qualified Data.List.Split as S
import System.Random
import GameBoard
import Orientation
import Pos
import Vessel

generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldCarrier, bldBattleship, bldCruiser, bldSubmarine, bldDestroyer]
  vessels <- buildVessels vesselBuilders bL tR []
  pure (GameBoard bL tR vessels [] [])

buildVessels :: [(Orientation -> Pos -> Vessel)] -> Pos -> Pos -> [Vessel] -> IO [Vessel]
buildVessels [] _ _ acc = pure acc
buildVessels (f:fs) bL tR acc = do
  vessel <- buildVessel f bL tR
  if overlapsAny vessel acc
    then buildVessels (f:fs) bL tR acc
    else buildVessels fs bL tR (vessel : acc)

buildVessel :: (Orientation -> Pos -> Vessel) -> Pos -> Pos -> IO Vessel
buildVessel f bL tR = do
  o <- randomOrient
  p <- randomPos bL tR
  let vessel = f o p
  if vesselOffBoard tR vessel
    then buildVessel f bL tR
    else pure vessel

overlapsAny :: Vessel -> [Vessel] -> Bool
overlapsAny _ [] = False
overlapsAny v1 (v2:vs) = overlapping || overlapsAny v1 vs
  where overlapping = do
          let v1Pos = positions v1
          let v2Pos = positions v2
          not $ null $ L.intersect v1Pos v2Pos

randomOrient :: IO Orientation
randomOrient = do
  r <- randomNumber (1,2)
  if r == 1
    then pure Vertical
    else pure Horizontal

randomPos :: Pos -> Pos -> IO Pos
randomPos (x1,y1) (x2,y2) = do
  x <- randomNumber (x1,x2)
  y <- randomNumber (y1,y2)
  pure (x,y)

randomNumber :: (Int, Int) -> IO Int
randomNumber bounds = do
  g <- getStdGen
  let r = randomR bounds g
  setStdGen $ snd r
  pure $ fst r

-- TODO handle invalid entry by the user - ie. don't fail catastrophically
strToPos :: String -> Pos
strToPos s = do
  let splitPos = S.splitOn "," s
  let x = read (splitPos!!0)
  let y = read (splitPos!!1)
  (x,y)

-- TODO figure out why if you enter null input before the first real shot the board seems to be regenerated (ie. the vessels are repositioned but generateBoard is not being called again - WTF!?!)
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
        then runGame ioBoard
        else do
          let shot = strToPos line
          let newBoard = takeShot board shot
          runGame newBoard

main :: IO ()
main = do
  putStrLn "Generating board ..."
  let board = generateBoard
  runGame board

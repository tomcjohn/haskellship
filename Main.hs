module Main where

import qualified Data.List as L
import qualified System.Console.ANSI as C
import System.Random
import Text.Parsec
import Text.Parsec.String

import GameBoard
import Orientation
import Pos
import Vessel

generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldSubmarine, bldDestroyer, bldCruiser, bldCarrier, bldBattleship]
  vs <- buildVessels vesselBuilders bL tR []
  pure $ GameBoard bL tR vs []

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

vesselOffBoard :: Pos -> Vessel -> Bool
vesselOffBoard tR v = do
  let lastPos = (last . positions) v :: Pos
  (fst lastPos) > (fst tR) || (snd lastPos) > (snd tR)

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

intParser :: Parser Int
intParser = read <$> many1 digit

-- move position parsing into Pos.hs
posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- oneOf ","
  y <- intParser
  pure (x,y)

runGame :: GameBoard -> IO ()
runGame board = do
  printBoard board
  if gameOver board
    then do
      putStrLn "Game Over"
      putStrLn "YOU WIN!!!"
    else do
      putStr "Take a shot (x,y): "
      line <- getLine
      C.clearScreen
      case parse posParser "" line of
        Left _ -> do
          putStrLn $ "Invalid input " ++ (show line)
          runGame board
        Right shot -> do
          -- consider replacing next two lines with "shoot board shot >>= runGame"
          newBoard <- shoot board shot
          runGame newBoard

main :: IO ()
main = do
  C.clearScreen
  putStrLn "Generating board ..."
  board <- generateBoard
  runGame board

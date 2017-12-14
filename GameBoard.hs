module GameBoard where

import Control.Monad
import Debug.Trace
import Position
import Vessel

data GameBoard = GameBoard Position Position [Vessel] deriving Show

printBoard :: GameBoard -> IO ()
printBoard (GameBoard (Position x1 y1) (Position x2 y2) vessels) = do
  printRows [x1..x2] [y1..y2]
  putStrLn "========================================="

printRows :: [Int] -> [Int] -> IO ()
printRows _ [] = do
  pure ()
printRows xs (y:ys) = do
  putStrLn "========================================="
  printRow y xs
  printRows xs ys

printRow :: Int -> [Int] -> IO ()
printRow y xs = do
  putStr "|"
  printSquares y xs
  putStrLn ""

printSquares :: Int -> [Int] -> IO ()
printSquares _ [] = do
  pure ()
printSquares y (x:xs) = do
  printSquare x y
  printSquares y xs

printSquare :: Int -> Int -> IO ()
printSquare x y = putStr $ " - |"

takeShot :: GameBoard -> Position -> IO GameBoard
takeShot (GameBoard p1 p2 vessels) shot = do
  if not $ onBoard p1 p2 shot
    then do
      putStrLn $ "Off board: " ++ (show shot)
      pure $ GameBoard p1 p2 vessels
    else do
      putStrLn $ "Shot: " ++ (show shot)
      pure $ GameBoard p1 p2 (shoot shot vessels)
      where shoot _ [] = []
            shoot p (v:vs) =
              if vesselContains p v
                then do
                  trace "HIT!" addHit p v : vs
                else do
                  trace "Miss" v : shoot p vs

onBoard :: Position -> Position -> Position -> Bool
onBoard (Position x1 y1) (Position x2 y2) (Position x y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> IO Bool
gameOver (GameBoard _ _ vessels) = do
  pure $ allSunk vessels

allSunk :: [Vessel] -> Bool
allSunk [] = True
allSunk (v:vs) = isSunk v && allSunk vs

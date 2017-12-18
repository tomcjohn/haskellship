module GameBoard where

import Debug.Trace
import Pos
import Vessel

data GameBoard = GameBoard Pos Pos [Vessel] [Pos] deriving Show

printBoard :: GameBoard -> IO ()
printBoard (GameBoard (x1,y1) (x2,y2) _ hits) = do
  printRows hits [x1..x2] [y1..y2]
  putStrLn "========================================="

printRows :: [Pos] -> [Int] -> [Int] -> IO ()
printRows _ [] _ = do
  pure ()
printRows _ _ [] = do
  pure ()
printRows hits xs (y:ys) = do
  putStrLn "========================================="
  printRow hits y xs
  printRows hits xs ys

printRow :: [Pos] -> Int -> [Int] -> IO ()
printRow hits y xs = do
  putStr "|"
  printSquares hits y xs
  putStrLn ""

printSquares :: [Pos] -> Int -> [Int] -> IO ()
printSquares _ _ [] = do
  pure ()
printSquares hits y (x:xs) = do
  printSquare hits x y
  printSquares hits y xs

printSquare :: [Pos] -> Int -> Int -> IO ()
printSquare hits x y = do
  putStr " "
  if elem (x,y) hits then putStr "X" else putStr "-"
  putStr "|"

takeShot :: GameBoard -> Pos -> IO GameBoard
takeShot (GameBoard p1 p2 afloat hits) shot = do
  if not $ onBoard p1 p2 shot
    then do
      putStrLn $ "Off board: " ++ (show shot)
      pure $ GameBoard p1 p2 afloat hits
    else do
      putStrLn $ "Shot: " ++ (show shot)
      -- TODO return a new GameBoard changed by the application of the shot!
      putStrLn "NOTHING HAPPENED!!!"
      pure $ GameBoard p1 p2 afloat hits

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> IO Bool
gameOver (GameBoard _ _ afloat _) = do
  pure $ null afloat

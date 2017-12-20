module GameBoard where

import Pos
import Vessel

data GameBoard = GameBoard Pos Pos [Vessel] [Pos] [Pos] deriving Show

-- TODO Flip board, ie. 0,0 should be bottom left when output
-- TODO Add cell indexes to board output
printBoard :: GameBoard -> IO ()
printBoard (GameBoard (x1,y1) (x2,y2) _ hits misses) = do
  printRows hits misses [x1..x2] [y1..y2]
  putStrLn "========================================="

printRows :: [Pos] -> [Pos] -> [Int] -> [Int] -> IO ()
printRows _ _ _ [] = do
  pure ()
printRows hits misses xs (y:ys) = do
  putStrLn "========================================="
  printRow hits misses y xs
  printRows hits misses xs ys

printRow :: [Pos] -> [Pos] -> Int -> [Int] -> IO ()
printRow hits misses y xs = do
  putStr "|"
  printSquares hits misses y xs
  putStrLn ""

printSquares :: [Pos] -> [Pos] -> Int -> [Int] -> IO ()
printSquares _ _ _ [] = do
  pure ()
printSquares hits misses y (x:xs) = do
  printSquare hits misses x y
  printSquares hits misses y xs

printSquare :: [Pos] -> [Pos] -> Int -> Int -> IO ()
printSquare hits misses x y = do
  putStr " "
  if elem (x,y) hits
    then putStr "X"
    else if elem (x,y) misses
      then putStr "-"
      else putStr " "
  putStr " |"

vesselOffBoard :: Pos -> Vessel -> Bool
vesselOffBoard tR v = do
  let lastPos = (last . positions) v :: Pos
  (fst lastPos) > (fst tR) || (snd lastPos) > (snd tR)

-- TODO add "You sunk my Battleship!" style messages when a vessel is sunk
takeShot :: GameBoard -> Pos -> IO GameBoard
takeShot (GameBoard bL tR vessels hits misses) shot = do
  if not $ onBoard bL tR shot
    then do
      putStrLn $ "Off board: " ++ (show shot)
      pure $ GameBoard bL tR vessels hits misses
    else do
      if elem shot hits || elem shot misses
        then do
          putStrLn "Ignoring repeat shot ..."
          pure $ GameBoard bL tR vessels hits misses
        else if didItHit vessels shot
          then do
            putStrLn "HIT!"
            pure $ GameBoard bL tR vessels (shot:hits) misses
          else do
            putStrLn "MISS!"
            pure $ GameBoard bL tR vessels hits (shot:misses)

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> IO Bool
gameOver (GameBoard _ _ vessels hits _) = do
  pure $ allSunk vessels hits

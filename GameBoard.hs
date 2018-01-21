module GameBoard where

import Control.Monad
import Pos
import Vessel

-- A gameboard is created from a bottom left position, a top right position,
-- the list of vessels on the board and the list of shots that were misses.
data GameBoard = GameBoard {
  bottomLeft::Pos,
  topRight::Pos,
  vessels::[Vessel],
  misses::[Pos]
} deriving Show

data ShotResult =
  OffBoard |
  RepeatShot |
  Hit [Vessel] |
  Miss

listHits :: [Vessel] -> [Pos]
listHits [] = []
listHits (v:vs) = vesselHits v ++ listHits vs

-- refactor the print functions below to minimise number of IO ()'s ie. printBoard should be the only one and the rest should return Strings - look at intercalate
printBoard :: GameBoard -> IO ()
printBoard (GameBoard (x1,y1) (x2,y2) vessels misses) = do
  let xs = [x1..x2]
  let ys = reverse [y1..y2]
  printRows (listHits vessels) misses xs ys
  putStr "  ="
  putStrLn (concat $ replicate ((maximum xs - minimum xs) + 1) "====")
  putStr "   "
  _ <- mapM (\x -> putStr $ " " ++ show x ++ "  ") xs
  putStrLn ""

printRows :: [Pos] -> [Pos] -> [Int] -> [Int] -> IO ()
printRows _ _ _ [] = do
  pure ()
printRows hits misses xs (y:ys) = do
  putStrLn "  ========================================="
  putStr $ (show y) ++ " "
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

-- try and get all IO into Main.hs!
shoot :: GameBoard -> Pos -> IO GameBoard
shoot board shot = do
  result <- takeShot board shot
  case result of
    OffBoard -> do
      putStrLn $ "Off board " ++ (show shot)
      pure board
    RepeatShot -> do
      putStrLn $ "Repeat shot " ++ (show shot)
      pure board
    Hit newVessels -> do
      putStrLn $ "HIT " ++ (show shot)
      pure $ board {vessels=newVessels}
    Miss -> do
      putStrLn $ "MISS " ++ (show shot)
      pure $ board {misses=(shot:misses board)}

takeShot :: GameBoard -> Pos -> IO ShotResult
takeShot (GameBoard bL tR vessels misses) shot = do
  if not $ onBoard bL tR shot
    then pure OffBoard
  else if elem shot misses
    then pure RepeatShot
  else
    doIt vessels shot []
  -- could try runWriter here to build a pair of (ShotResult, [Vessel]) (avoids the need for doIt accumulator and all the list concatenation)
  where doIt [] _ _ = pure Miss
        doIt (v:vs) s acc = do
          if elem s (vesselHits v)
            then pure RepeatShot
          else if isHit s v
            then do
              let newVessel = addHit s v
              when (isSunk newVessel) $ putStrLn $ "You sunk my " ++ vesselType newVessel ++ "!"
              -- add field to Hit to contain Maybe sunk vessel (needed to move this IO out of here and back into Main)
              pure $ Hit (acc ++ [addHit s v] ++ vs)
          else
            doIt vs s (acc ++ [v])

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> Bool
gameOver board =
  allSunk $ vessels board

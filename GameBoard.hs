module GameBoard where

import Control.Monad
import Pos
import Vessel

-- A gameboard is created from a bottom left position, a top right position
-- the list of vessels on the board and the list of shot positions that were misses.
data GameBoard = GameBoard Pos Pos [Vessel] [Pos] deriving Show

data ShotResult =
  OffBoard |
  RepeatShot |
  Hit [Vessel] |
  Miss

listHits :: [Vessel] -> [Pos]
listHits [] = []
listHits (v:vs) = vesselHits v ++ listHits vs

-- TODO look into terminal escape characters so board redisplays rather than scrolling in the window
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

vesselOffBoard :: Pos -> Vessel -> Bool
vesselOffBoard tR v = do
  let lastPos = (last . positions) v :: Pos
  (fst lastPos) > (fst tR) || (snd lastPos) > (snd tR)

takeShot :: GameBoard -> Pos -> IO GameBoard
takeShot board@(GameBoard bL tR vessels misses) shot = do
  result <- shoot board shot
  case result of
    OffBoard -> do
      putStrLn $ "Off board: " ++ (show shot)
      pure $ board
    RepeatShot -> do
      putStrLn $ "Ignoring repeat shot: " ++ (show shot)
      pure $ board
    Hit newVessels -> do
      putStrLn "HIT!"
      pure $ GameBoard bL tR newVessels misses
    Miss -> do
      putStrLn "MISS!"
      pure $ GameBoard bL tR vessels (shot:misses)

shoot :: GameBoard -> Pos -> IO ShotResult
shoot board@(GameBoard bL tR _ misses) shot = do
  if not $ onBoard bL tR shot
    then pure OffBoard
  else if elem shot misses
    then pure RepeatShot
  else
    checkForHit board shot

checkForHit :: GameBoard -> Pos -> IO ShotResult
checkForHit board shot = doIt board shot []
  where doIt (GameBoard _ _ [] _) _ _ = pure Miss
        doIt (GameBoard bL tR (v:vs) misses) s acc = do
          if elem s (vesselHits v)
            then pure RepeatShot
          else if isHit s v
            then do
              let newVessel = addHit s v
              when (isSunk newVessel) $ putStrLn $ "You sunk my " ++ vesselType newVessel ++ "!"
              pure $ Hit (acc ++ [addHit s v] ++ vs)
          else
            doIt (GameBoard bL tR vs misses) s (acc ++ [v])

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> IO Bool
gameOver (GameBoard _ _ vessels _) =
  pure $ allSunk vessels

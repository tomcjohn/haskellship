module GameBoard where

import Control.Monad
import qualified Data.Set as Set
import Pos
import Vessel

-- A gameboard is created from a bottom left position, a top right position,
-- the list of vessels on the board and the list of shots that were misses.
data GameBoard = GameBoard {
  bottomLeft::Pos,
  topRight::Pos,
  vessels::[Vessel],
  misses::PosSet
} deriving Show

data ShotResult =
  OffBoard |
  RepeatShot |
  Hit [Vessel] |
  Miss

listHits :: [Vessel] -> PosSet
listHits [] = Set.empty
listHits (v:vs) = Set.union (hits v) (listHits vs)

-- TODO refactor the print functions below to minimise number of IO ()'s
-- ie. printBoard should be the only one and the rest should return Strings - look at intercalate
printBoard :: GameBoard -> IO ()
printBoard (GameBoard (x1,y1) (x2,y2) vs ms) = do
  let xs = [x1..x2]
  let ys = reverse [y1..y2]
  printRows (listHits vs) ms xs ys
  putStr "  ="
  putStrLn (concat $ replicate ((maximum xs - minimum xs) + 1) "====")
  putStr "   "
  _ <- mapM (\x -> putStr $ " " ++ show x ++ "  ") xs
  putStrLn ""

printRows :: PosSet -> PosSet -> [Int] -> [Int] -> IO ()
printRows _ _ _ [] = do
  pure ()
printRows hs ms xs (y:ys) = do
  putStrLn "  ========================================="
  putStr $ (show y) ++ " "
  printRow hs ms y xs
  printRows hs ms xs ys

printRow :: PosSet -> PosSet -> Int -> [Int] -> IO ()
printRow hs ms y xs = do
  putStr "|"
  printSquares hs ms y xs
  putStrLn ""

printSquares :: PosSet -> PosSet -> Int -> [Int] -> IO ()
printSquares _ _ _ [] = do
  pure ()
printSquares hs ms y (x:xs) = do
  printSquare hs ms x y
  printSquares hs ms y xs

printSquare :: PosSet -> PosSet -> Int -> Int -> IO ()
printSquare hs ms x y = do
  putStr " "
  if elem (x,y) hs
    then putStr "X"
    else if elem (x,y) ms
      then putStr "-"
    else putStr " "
  putStr " |"

-- TODO try and get all IO into Main.hs!
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
      pure $ board {misses=(Set.insert shot (misses board))}

takeShot :: GameBoard -> Pos -> IO ShotResult
takeShot (GameBoard bL tR vs ms) shot = do
  if not $ onBoard bL tR shot
    then pure OffBoard
  else if elem shot ms
    then pure RepeatShot
  else
    doIt vs shot []
  -- TODO could try runWriter here to build a pair of (ShotResult, [Vessel]) (avoids the need for doIt accumulator and all the list concatenation)
  where doIt [] _ _ = pure Miss
        doIt (v:vRest) s acc = do
          if elem s (hits v)
            then pure RepeatShot
          else if isHit s v
            then do
              let newVessel = addHit s v
              when (isSunk newVessel) $ putStrLn $ "You sunk my " ++ show (vesselType newVessel) ++ "!"
              -- TODO add field to Hit to contain Maybe sunk vessel (needed to move this IO out of here and back into Main)
              pure $ Hit (acc ++ [addHit s v] ++ vRest)
          else
            doIt vRest s (acc ++ [v])

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> Bool
gameOver board =
  allSunk $ vessels board

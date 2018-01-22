module GameBoard where

import Control.Monad
import qualified Data.Set as Set
import qualified System.Random as Rand
import Orientation
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

generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldSubmarine, bldDestroyer, bldCruiser, bldCarrier, bldBattleship]
  vs <- buildVessels vesselBuilders bL tR []
  pure $ GameBoard bL tR vs Set.empty

buildVessels :: [Orientation -> Pos -> Vessel] -> Pos -> Pos -> [Vessel] -> IO [Vessel]
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
vesselOffBoard tR vessel = do
  let lastPos = last $ Set.elems $ positions vessel
  (fst lastPos > fst tR) || (snd lastPos > snd tR)

overlapsAny :: Vessel -> [Vessel] -> Bool
overlapsAny _ [] = False
overlapsAny v1 (v2:vs) = overlapping || overlapsAny v1 vs
  where overlapping = do
          let v1Pos = positions v1
          let v2Pos = positions v2
          not $ null $ Set.intersection v1Pos v2Pos

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
  g <- Rand.getStdGen
  let r = Rand.randomR bounds g
  Rand.setStdGen $ snd r
  pure $ fst r

-- TODO refactor the print functions below to minimise number of IO ()'s
-- ie. printBoard should be the only one and the rest should return Strings - look at intercalate
printBoard :: GameBoard -> IO ()
printBoard (GameBoard (x1,y1) (x2,y2) vs ms) = do
  let xs = [x1..x2]
  let ys = reverse [y1..y2]
  let strs = printRows (listHits vs) ms xs ys
  printStrings strs
  putStr "  ="
  putStrLn (concat $ replicate ((maximum xs - minimum xs) + 1) "====")
  putStr "   "
  _ <- mapM (\x -> putStr $ " " ++ show x ++ "  ") xs
  putStrLn ""

printStrings :: [String] -> IO ()
printStrings [] = pure ()
printStrings (s:ss) = do
  putStrLn s
  printStrings ss

printRows :: PosSet -> PosSet -> [Int] -> [Int] -> [String]
printRows _ _ _ [] = []
printRows hs ms xs (y:ys) = do
  let rowHeader = "  ========================================="
  let row = show y ++ " " ++ printRow hs ms y xs
  rowHeader : (row : printRows hs ms xs ys)

printRow :: PosSet -> PosSet -> Int -> [Int] -> String
printRow hs ms y xs =
  "|" ++ printSquares hs ms y xs

printSquares :: PosSet -> PosSet -> Int -> [Int] -> String
printSquares _ _ _ [] = ""
printSquares hs ms y (x:xs) =
  printSquare hs ms x y ++ printSquares hs ms y xs

printSquare :: PosSet -> PosSet -> Int -> Int -> String
printSquare hs ms x y
  | (x,y) `elem` hs = " X |"
  | (x,y) `elem` ms = " - |"
  | otherwise = "   |"

-- TODO try and get all IO into Main.hs!
shoot :: GameBoard -> Pos -> IO GameBoard
shoot board shot = do
  result <- takeShot board shot
  case result of
    OffBoard -> do
      putStrLn $ "Off board " ++ show shot
      pure board
    RepeatShot -> do
      putStrLn $ "Repeat shot " ++ show shot
      pure board
    Hit newVessels -> do
      putStrLn $ "HIT " ++ show shot
      pure $ board {vessels=newVessels}
    Miss -> do
      putStrLn $ "MISS " ++ show shot
      pure $ board {misses=Set.insert shot (misses board)}

takeShot :: GameBoard -> Pos -> IO ShotResult
takeShot (GameBoard bL tR vs ms) shot =
  if not $ onBoard bL tR shot
    then pure OffBoard
  else if shot `elem` ms
    then pure RepeatShot
  else
    doIt vs shot []
  -- TODO could try runWriter here to build a pair of (ShotResult, [Vessel]) (avoids the need for doIt accumulator and all the list concatenation)
  where doIt [] _ _ = pure Miss
        doIt (v:vRest) s acc =
          if s `elem` hits v
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

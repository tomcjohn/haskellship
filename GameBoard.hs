module GameBoard
  ( GameBoard (..)
  , ShotResult (..)
  , generateBoard
  , renderBoard
  , takeShot
  , gameOver
  ) where

import qualified Data.List as List
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
  Hit GameBoard (Maybe Vessel) |
  Miss GameBoard

generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldSubmarine, bldDestroyer, bldCruiser, bldCarrier, bldBattleship]
  vs <- buildVessels vesselBuilders bL tR []
  pure $ GameBoard bL tR vs empty

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
  let lastPos = last $ elems $ positions vessel
  (fst lastPos > fst tR) || (snd lastPos > snd tR)

overlapsAny :: Vessel -> [Vessel] -> Bool
overlapsAny _ [] = False
overlapsAny v1 (v2:vs) = overlapping || overlapsAny v1 vs
  where overlapping = do
          let v1Pos = positions v1
          let v2Pos = positions v2
          not $ null $ intersection v1Pos v2Pos

renderBoard :: GameBoard -> String
renderBoard board@(GameBoard (x1,y1) (x2,y2) _ _) = do
  let xs = [x1..x2]
  let ys = reverse [y1..y2]
  concat $ fmap (++ "\n") $ printRows ys xs board ++ (finalRow xs)

printRows :: [Int] -> [Int] -> GameBoard -> [String]
printRows [] _ _ = []
printRows (y:ys) xs board = do
  printRow y xs board ++ printRows ys xs board

printRow :: Int -> [Int] -> GameBoard -> [String]
printRow y xs board = do
  let rowBody = show y ++ " |" ++ printSquares y xs board
  [rowHeader xs, rowBody]

rowHeader :: [Int] -> String
rowHeader range = "  =" ++ (concat $ replicate ((maximum range - minimum range) + 1) "====")

printSquares :: Int -> [Int] -> GameBoard -> String
printSquares _ [] _ = ""
printSquares y (x:xs) board =
  printSquare y x board ++ printSquares y xs board

printSquare :: Int -> Int -> GameBoard -> String
printSquare y x board = do
  let hs = listHits (vessels board)
  let ms = misses board
  let char = if (x,y) `elem` hs
               then "X"
               else if (x,y) `elem` ms
                 then "-"
                 else " "
  List.intercalate char [" ", " |"]

finalRow :: [Int] -> [String]
finalRow xs =
  let xIndexes = "   " ++ (concat $ fmap (\x -> " " ++ show x ++ "  ") xs)
  in [rowHeader xs, xIndexes]

takeShot :: GameBoard -> Pos -> ShotResult
takeShot board shot =
  if not $ onBoard (bottomLeft board) (topRight board) shot
    then OffBoard
  else if shot `elem` (misses board)
    then RepeatShot
  else
    doIt (vessels board) shot []
  -- TODO could try runWriter here to build a pair of (ShotResult, [Vessel]) (avoids the need for doIt accumulator and all the list concatenation)
  where doIt [] _ _ = Miss $ board {misses=insert shot (misses board)}
        doIt (v:vs) s acc =
          if s `elem` hits v
            then RepeatShot
          else if isHit s v
            then do
              let newVessel = addHit s v
              let newVessels = acc ++ [newVessel] ++ vs
              Hit (board {vessels=newVessels})
                (if isSunk newVessel then Just newVessel else Nothing)
          else
            doIt vs s (acc ++ [v])

onBoard :: Pos -> Pos -> Pos -> Bool
onBoard (x1,y1) (x2,y2) (x,y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

gameOver :: GameBoard -> Bool
gameOver board =
  allSunk $ vessels board

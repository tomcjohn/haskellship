module GameBoard where

import qualified Data.List as List
import Orientation
import Pos
import Vessel

data Cell = Cell
  { vesselType::VesselType
  , hitType::HitType
  } deriving Show

data HitType = Hit | Miss | Empty deriving Show

data GameBoard = GameBoard
  { bottomLeft::Pos
  , topRight::Pos
  , cells::[[Cell]]
  } deriving Show

generateBoard :: IO GameBoard
generateBoard = do
  let bL = (0,0)
  let tR = (9,9)
  let vesselBuilders = [bldSubmarine, bldDestroyer, bldCruiser, bldCarrier, bldBattleship]
  vs <- buildVessels vesselBuilders bL tR []
  print vs
  pure $ GameBoard bL tR $ createCells vs bL tR

createCells :: [PositionedVessel] -> Pos -> Pos -> [[Cell]]
createCells vs (x1,y1) (x2,y2) = do
  let xs = [x1..x2]
  let ys = [y1..y2]
  createRows vs xs ys

createRows :: [PositionedVessel] -> [Int] -> [Int] -> [[Cell]]
createRows _  _  []     = []
createRows vs xs (y:ys) = createRow vs xs y : createRows vs xs ys

createRow :: [PositionedVessel] -> [Int] -> Int -> [Cell]
createRow _  []     _ = []
createRow vs (x:xs) y = createCell vs x y : createRow vs xs y

createCell :: [PositionedVessel] -> Int -> Int -> Cell
createCell vs x y = Cell (hasVesselType x y vs) Empty

-- TODO this is very wasteful as it does a full scan of the [PositionedVessel] for every cell on the gameboard - it'll do for now
hasVesselType :: Int -> Int -> [PositionedVessel] -> VesselType
hasVesselType _ _ [] = NoVessel
hasVesselType x y (v:vs) =
  if (x,y) `elem` positions v
    then vType v
    else hasVesselType x y vs

buildVessels :: [Orientation -> Pos -> PositionedVessel] -> Pos -> Pos -> [PositionedVessel] -> IO [PositionedVessel]
buildVessels [] _ _ acc = pure acc
buildVessels (f:fs) bL tR acc = do
  vessel <- buildVessel f bL tR
  if overlapsAny vessel acc
    then buildVessels (f:fs) bL tR acc
    else buildVessels fs bL tR (vessel : acc)

buildVessel :: (Orientation -> Pos -> PositionedVessel) -> Pos -> Pos -> IO PositionedVessel
buildVessel f bL tR = do
  o <- randomOrient
  p <- randomPos bL tR
  let vessel = f o p
  if vesselOffBoard tR vessel
    then buildVessel f bL tR
    else pure vessel

vesselOffBoard :: Pos -> PositionedVessel -> Bool
vesselOffBoard tR vessel = do
  let lastPos = last $ elems $ positions vessel
  (fst lastPos > fst tR) || (snd lastPos > snd tR)

overlapsAny :: PositionedVessel -> [PositionedVessel] -> Bool
overlapsAny _ [] = False
overlapsAny v1 (v2:vs) = overlapping || overlapsAny v1 vs
  where overlapping = do
          let v1Pos = positions v1
          let v2Pos = positions v2
          not $ null $ intersection v1Pos v2Pos

renderBoard :: GameBoard -> String
renderBoard board@(GameBoard (x1,y1) (x2,y2) _) = do
  let xs = [x1..x2]
  let ys = reverse [y1..y2]
  concat $ fmap (++ "\n") $ renderRows ys xs board ++ (finalRow xs)

rowHeader :: [Int] -> String
rowHeader range = "  =" ++ (concat $ replicate ((maximum range - minimum range) + 1) "====")

renderRows :: [Int] -> [Int] -> GameBoard -> [String]
renderRows [] _ _ = []
renderRows (y:ys) xs board = do
  renderRow y xs board ++ renderRows ys xs board

renderRow :: Int -> [Int] -> GameBoard -> [String]
renderRow y xs board = do
  let rowBody = show y ++ " |" ++ renderCells y xs board
  [rowHeader xs, rowBody]

renderCells :: Int -> [Int] -> GameBoard -> String
renderCells _ [] _ = ""
renderCells y (x:xs) board =
  renderCell (hitType((cells board)!!y!!x)) ++ renderCells y xs board

renderCell :: HitType -> String
renderCell ht = List.intercalate (toString ht) [" ", " |"]
  where toString Hit   = "X"
        toString Miss  = "-"
        toString Empty = " "

finalRow :: [Int] -> [String]
finalRow xs =
  let xIndexes = "   " ++ (concat $ fmap (\x -> " " ++ show x ++ "  ") xs)
  in [rowHeader xs, xIndexes]

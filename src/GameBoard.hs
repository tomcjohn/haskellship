module GameBoard where

import Orientation
import Pos
import Vessel

data Cell = Cell
  { vType::VesselType
  , hType::HitType
  } deriving Show

data HitType = Hit | Miss deriving Show

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
  pure $ GameBoard bL tR []

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

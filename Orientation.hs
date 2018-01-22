module Orientation where

import System.Random

data Orientation = Vertical | Horizontal deriving (Bounded, Enum, Show)

instance Random Orientation where
  random g = case randomR (fromEnum (minBound :: Orientation), fromEnum (maxBound :: Orientation)) g of
               (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')

randomOrient :: IO Orientation
randomOrient = do
  g <- getStdGen
  let r = random g
  pure $ fst r

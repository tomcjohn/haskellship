module Pos where

import           Data.Set           (Set)
import qualified Data.Set           as S
import qualified System.Random      as R
import           Text.Parsec
import           Text.Parsec.String

type Pos = (Int, Int)
type PosSet = Set Pos

randomPos :: Pos -> Pos -> IO Pos
randomPos (x1,y1) (x2,y2) = do
  x <- randomNumber (x1,x2)
  y <- randomNumber (y1,y2)
  pure (x,y)

randomNumber :: (Int, Int) -> IO Int
randomNumber bounds = do
  g <- R.getStdGen
  let r = R.randomR bounds g
  R.setStdGen $ snd r
  pure $ fst r

posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- oneOf ","
  y <- intParser
  pure (x,y)

intParser :: Parser Int
intParser = read <$> many1 digit

empty :: PosSet
empty = S.empty

fromList :: [Pos] -> PosSet
fromList = S.fromList

elems :: PosSet -> [Pos]
elems = S.elems

insert :: Pos -> PosSet -> PosSet
insert = S.insert

union :: PosSet -> PosSet -> PosSet
union = S.union

intersection :: PosSet -> PosSet -> PosSet
intersection = S.intersection

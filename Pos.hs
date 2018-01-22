module Pos where

-- TODO can we reduce the proliferation of Set logic across Pos, Vessel and GameBoard now?
import Data.Set (Set)
import qualified System.Random as Rand
import Text.Parsec
import Text.Parsec.String

type Pos = (Int, Int)
type PosSet = Set Pos

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

posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- oneOf ","
  y <- intParser
  pure (x,y)

intParser :: Parser Int
intParser = read <$> many1 digit

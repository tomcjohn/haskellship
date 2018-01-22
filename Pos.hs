module Pos where

-- TODO can we reduce the proliferation of Set logic across Pos, Vessel and GameBoard now?
import Data.Set (Set)
import Text.Parsec
import Text.Parsec.String

type Pos = (Int, Int)
type PosSet = Set Pos

posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- oneOf ","
  y <- intParser
  pure (x,y)

intParser :: Parser Int
intParser = read <$> many1 digit

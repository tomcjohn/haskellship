module Pos where

import Text.Parsec
import Text.Parsec.String

type Pos = (Int, Int)

posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- oneOf ","
  y <- intParser
  pure (x,y)

intParser :: Parser Int
intParser = read <$> many1 digit

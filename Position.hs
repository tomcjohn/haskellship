module Position where

data Position = Position Int Int deriving Show

class HasPosition a where
  pos :: a -> Position

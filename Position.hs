module Position where

data Position = Position Int Int

class HasPosition a where
  pos :: a -> Position

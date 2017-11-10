module Position where

data Position = Position Int Int deriving (Eq,Ord,Show)

class HasPosition a where
  pos :: a -> Position

module Jammin where

import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam Fruit Int
  deriving (Eq, Show)

data JamJars2 = Jam2 { fruit :: Fruit, qty :: Int }
  deriving (Eq, Show, Ord)

row1 = [Jam2 Peach 3, Jam2 Plum 3, Jam2 Blackberry 2]
row2 = [Jam2 Apple 5]
row3 = [Jam2 Blackberry 7, Jam2 Apple 1]
row4 = [Jam2 Plum 1, Jam2 Apple 2, Jam2 Peach 3]
row5 = [Jam2 Peach 1]
row6 = [Jam2 Apple 1]
allJam = [row1, row2, row3, row4, row5, row6]

mostRow :: [[JamJars2]] -> JamJars2
mostRow = foldr1 (\x acc -> if qty x > qty acc then x else acc ) . concat 

compareKind (Jam2 k _) (Jam2 k' _) = compare k k'
  

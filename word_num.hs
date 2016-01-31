module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine" 

digits :: Int -> [Int]
digits n = go n []
  where go n result
         | n < 10 = n : result
         | otherwise = go (n `div` 10) (n `mod` 10 : result) 

wordNumber :: Int -> String
wordNumber n = concat $ intersperse ['-'] $ map digitToWord $ digits n

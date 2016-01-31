myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo a b
  | a > b = []  
  | a == b = [a]
  | a < b = a : myEnumFromTo (succ a) b

myWords :: String -> [String]
myWords a = go a []
  where go a result
         | a == [] = result 
         | otherwise = go (dropWhile (== ' ') $ dropWhile (/= ' ') a) (result ++ [takeWhile (/= ' ') a]) 

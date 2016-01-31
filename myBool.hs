myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f a = myOr $ map (f) a

myElem :: Eq a => a -> [a] -> Bool
myElem a b = myAny (\x -> x == a) b

myReverse :: [a] -> [a]
myReverse a = go a [] 
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)

squish :: [[a]] -> [a]
squish = go []
  where
    go acc [] = acc
    go acc (x:xs) = (go2 x acc) ++ (go acc xs) 
      where
        go2 acc [] = acc
        go2 acc (y:ys) = y : go2 ys acc

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = go f []
  where
    go f acc [] = acc
    go f acc (x:xs) = go f (acc ++ f x) xs
    
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (go []) 
  where
    go acc [] = acc
    go acc (x:xs) = x : go xs acc  

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = startGoList f 

startGoList f (x:xs) = goList f x xs

goList f acc (x:xs) = goList f (go f acc x) xs
goList f acc [] = acc

go :: (a -> a -> Ordering) -> a -> a -> a
go f a b = case f a b of
          GT -> a
          _  -> b 

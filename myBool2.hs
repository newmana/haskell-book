myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem a b = myAny (\x -> x == a) b

myElem' :: Eq a => a -> [a] -> Bool
myElem' a b = foldr (\x acc -> acc || x == a) False b

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []  

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> if f x then (x:) else id) [] 

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) [] 
    
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy f (x:xs) = foldr (\x acc -> gtGo f acc x) x xs
myMaximumBy f x = foldr1 (gtGo f) x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\x acc -> ltGo f acc x) x xs 

gtGo :: (a -> a -> Ordering) -> a -> a -> a
gtGo f a b = case f a b of
          GT -> a
          _  -> b 

ltGo :: (a -> a -> Ordering) -> a -> a -> a
ltGo f a b = case f a b of
          GT -> b
          _  -> a

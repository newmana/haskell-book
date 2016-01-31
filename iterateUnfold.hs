myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : myIterate f (f x) 

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f x = checkStuff f (f x)
  where
    checkStuff _ Nothing = []
    checkStuff f (Just (a, b)) = a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (f a, f a))
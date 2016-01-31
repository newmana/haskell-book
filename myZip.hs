import Data.Char

zip' :: [a] -> [b] -> [(a, b)]
zip' a b = go a b []
  where 
    go (x:xs) (y:ys) c = (x, y) : go xs ys c
    go _ _ c = c

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f a b = go f a b []
  where
    go f (x:xs) (y:ys) c = (f x y) : (go f xs ys c)
    go f _ _ c = c

capitalise (x:xs) = (toUpper x) : xs

makeAllCapital (x:xs) = (toUpper x) : (makeAllCapital xs)
makeAllCapital _ = []

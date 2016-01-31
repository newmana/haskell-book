lefts' :: [Either a b] -> [a]
lefts' = foldr matchLeft []
  where
    matchLeft (Left a) acc = a : acc
    matchLeft _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr matchRight []
  where
    matchRight (Right a) acc = a : acc
    matchRight _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' a = (lefts' a, rights' a)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f (Left a) = Nothing

either':: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b 

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\x -> Nothing) (\x -> Just (f x)) e 
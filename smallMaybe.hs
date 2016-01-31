isJust :: Maybe a -> Bool
isJust = maybe False (const True)

isNothing :: Maybe a -> Bool
isNothing = not . isJust

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' a _ Nothing = a
maybe' _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x a -> maybeToList x ++ a) [] 
  where
      justOrNothing Nothing a = a
      justOrNothing (Just x) a = x : a

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe a = case doCats [] a of
    [] -> Nothing
    a -> Just a
  where
    doCats acc [] = acc
    doCats acc (Just x : xs) = doCats (acc ++ [x]) xs
    doCats acc (Nothing : xs) = []
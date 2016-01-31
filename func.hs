funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd n =
  case even n of
    True -> n + 2
    _ -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ ->0


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero deriving (Show)

divideBy'' :: Numerator -> Denominator -> DividedResult
divideBy'' num denom = go (abs num) (abs denom) 0
  where go n d count
         | d == 0 = DividedByZero
         | n < d = makeResult count
         | otherwise = go (n - d) d (count + 1)
         where makeResult count
                | (num < 0 && denom < 0) || (num > 0 && denom > 0) = Result count
                | otherwise = Result (-count)

sums :: (Eq a, Num a) => a -> a
sums num 
      | num == 0 = 0
      | otherwise = num + (sums (num - 1))

recMulti :: (Eq a, Num a) => a -> a -> a
recMulti x y = go x y 0
  where go x y total
         | y == 0 = total
         | otherwise = go x (y - 1) (total + x)

mc91 n
  | n > 100 = n - 10
  | n <= 100 = mc91 $ mc91 (n + 11)

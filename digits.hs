tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit1a :: Integral a => a -> a
tensDigit1a x = d
  where xLast = fst $ divMod x 10
        d = snd $ divMod xLast 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where d2 = xLast `mod` 100
        xLast = x `div` 100

foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
  True -> x
  False -> y 

g :: (a -> b) -> (a, c) -> (b, c)
g toB (a, c) = (toB a, c)


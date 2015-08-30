module Chapter4Section7 where
  
awesome = ["Papuchon", "curry", "Haskell"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)

myAbs :: Integer -> Integer 
myAbs x = if x < 0 then (-x) else x 

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))
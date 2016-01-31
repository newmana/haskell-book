module PoemLines where

myWords :: String -> [String]
myWords = gen ' ' 

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
myLines = gen '\n'

gen :: Eq a => a -> [a] -> [[a]]
gen a b = go a b []
  where go a b result
         | b == [] = result
         | otherwise = go a (dropWhile (== a) $ dropWhile (/= a) b) (result ++ [takeWhile (/= a) b])

-- This is what we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

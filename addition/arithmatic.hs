module Arithmatic where 

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t) 
        go y (Just x, t) = (Just y, x >= y)

main :: IO () 
main = hspec $ do
  describe "Arithmatic" $ do
      it "half" $ do
        property $ \x -> (halfIdentity x) == (x :: Double)
      it "list" $ do
        property $ \x -> listOrdered (sort x::[Int]) == True
       
    --   it "1 + 1 is greater than 1" $ do
    --     (1 + 1) > 1 `shouldBe` True
        
    --   it "2 + 2 is equal to 4" $ do 
    --     2 + 2 `shouldBe` 4
        
    --   it "15 divided by 3 is 5" $ do
    --     dividedBy 15 3 `shouldBe` (5, 0)
        
    --   it "22 divided by 5 is 4 remainder 2" $ do
    --     dividedBy 22 5 `shouldBe` (4, 2)
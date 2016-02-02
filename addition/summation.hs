module Summation where

import Test.Hspec
import Test.QuickCheck

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering 
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b) 
genTuple = do
  a <- arbitrary 
  b <- arbitrary 
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do 
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b) 
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a) 
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a) 
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing), (99, return (Just a))]

recMulti :: (Eq a, Num a) => a -> a -> a
recMulti x y = go x y 0
  where go x y total
         | y == 0 = total
         | otherwise = go x (y - 1) (total + x)
         
main :: IO () 
main = hspec $ do
  describe "Multiplicate" $ do 
      
      it "2 * 2 is greater than 2" $ do
        (recMulti 2 2) > 2 `shouldBe` True
        
      it "2 * 2 is equal to 4" $ do 
        2 * 2 `shouldBe` 4
        
      it "x * 0 is always 0" $ do
        property $ \x -> x * 0 == (0 :: Int)

      it "x * 1 is always x" $ do
        property $ \x -> x * 1 == (x :: Int)

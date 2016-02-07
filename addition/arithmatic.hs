{-# LANGUAGE ScopedTypeVariables #-}

module Arithmatic where 

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List (sort)

half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t) 
        go y (Just x, _) = (Just y, x >= y)

main :: IO () 
main = hspec $ do
  describe "Arithmatic" $ do
      it "half" $ do
        property $ \(x::Double) -> (halfIdentity x) == x
      it "list" $ do
        property $ \(x::[Int]) -> listOrdered (sort x) == True
      it "plus associative" $ do
        property $ \((x,y,z)::(Int,Int,Int)) -> x + (y + z) == (x + y) + z 
      it "plus commute" $ do
        property $ \((x,y)::(Int,Int)) -> x + y == y + x
      it "mult associative" $ do
        property $ \((x,y,z)::(Int,Int,Int)) -> x * (y * z) == (x * y) * z 
      it "mult commute" $ do
        property $ \((x,y)::(Int,Int)) -> x * y == y * x 
      it "quot commute" $ do
        property $ \((NonZero x, NonZero y)::(NonZero Int, NonZero Int)) -> (quot x y) * y + (rem x y) == x 
      it "div commute" $ do
        property $ \((NonZero x, NonZero y)::(NonZero Int, NonZero Int)) -> (div x y) * y + (mod x y) == x 
      it "^ associative" $ do
        property $ \((x,y,z)::(Int,Int,Int)) -> x ^ (y ^ z) == (x ^ y) ^ z
      it "^ commute" $ do
        property $ \((x,y)::(Int,Int)) -> x ^ y == y ^ x
      it "same twice list" $ do
        property $ \(x::[Int]) -> (reverse . reverse) x == id x
                        
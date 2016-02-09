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
  describe "arithmetic" $ do
      it "should be equal when we halve the number and multiply it by two" $ do
        property $ \(x::Double) -> (halfIdentity x) == x
      it "should order a list" $ do
        property $ \(x::[Int]) -> listOrdered (sort x) == True
      it "is associative for addition" $ do
        property $ \((x,y,z)::(Int,Int,Int)) -> x + (y + z) == (x + y) + z 
      it "is commutative for addition" $ do
        property $ \((x,y)::(Int,Int)) -> x + y == y + x
      it "is associative for multiplication" $ do
        property $ \((x,y,z)::(Int,Int,Int)) -> x * (y * z) == (x * y) * z 
      it "is commutative for multiplication" $ do
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
      it "application" $ do
        property $ \((f, x)::(Fun Int Int, Int)) -> (apply f x) == (apply f $ x)
      it "application composition" $ do
        property $ \((f, g, x)::(Fun Int Int, Fun Int Int, Int)) -> ((apply f) . (apply g)) x == (apply f) ((apply g) x)
      it "foldr append" $ do
        property $ \(x::[Int],y::[Int]) -> foldr (:) y x == x ++ y
      it "foldr concat" $ do
        property $ \(x::[[Int]]) -> foldr (++) [] x == concat x
      it "length" $ do
        property $ \(x::[Int],y::Int) -> length (take y x) == y
      it "show" $ do
        property $ \(x::Int) -> (read (show x)) == x
      
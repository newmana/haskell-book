{-# LANGUAGE ScopedTypeVariables #-}

module Arithmatic where

import           Control.Applicative
import           Data.Char
import           Data.List                 (sort)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Modifiers

half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

square x = x * x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = liftA2 (:) (toUpper . head) tail

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGen' :: Gen Fool
foolGen' = frequency [ (2, return Fulse), (1, return Frue)]

main :: IO ()
main = hspec $
  describe "arithmatic" $
      it "should be equal when we halve the number and multiply it by two" $
        property $ \(x::Double) -> halfIdentity x == x
      it "should order a list" $
        property $ \(x::[Int]) -> listOrdered (sort x) == True
      it "is associative for addition" $
        property $ \((x,y,z)::(Int,Int,Int)) -> x + (y + z) == (x + y) + z
      it "is commutative for addition" $
        property $ \((x,y)::(Int,Int)) -> x + y == y + x
      it "is associative for multiplication" $
        property $ \((x,y,z)::(Int,Int,Int)) -> x * (y * z) == (x * y) * z
      it "is commutative for multiplication" $
        property $ \((x,y)::(Int,Int)) -> x * y == y * x
      it "quot commute" $
        property $ \((NonZero x, NonZero y)::(NonZero Int, NonZero Int)) -> quot x y * y + (rem x y) == x
      it "div commute" $
        property $ \((NonZero x, NonZero y)::(NonZero Int, NonZero Int)) -> (div x y) * y + (mod x y) == x
      it "^ associative" $
        property $ \((x,y,z)::(Int,Int,Int)) -> x ^ (y ^ z) == (x ^ y) ^ z
      it "^ commute" $
        property $ \((x,y)::(Int,Int)) -> x ^ y == y ^ x
      it "same twice list" $
        property $ \(x::[Int]) -> (reverse . reverse) x == id x
      it "application" $
        property $ \((f, x)::(Fun Int Int, Int)) -> (apply f x) == (apply f $ x)
      it "application composition" $
        property $ \((f, g, x)::(Fun Int Int, Fun Int Int, Int)) -> ((apply f) . (apply g)) x == (apply f) ((apply g) x)
      it "foldr append" $
        property $ \(x::[Int],y::[Int]) -> foldr (:) y x == x ++ y
      it "foldr concat" $
        property $ \(x::[[Int]]) -> foldr (++) [] x == concat x
      it "length" $
        property $ \(x::[Int],y::Int) -> length (take y x) == y
      it "show" $
        property $ \(x::Int) -> (read (show x)) == x
      it "square" $
        property $ \(x::Double) -> (square . sqrt) x == x
      it "idem capitalize 1" $
        property $ \((NonEmpty x)::(NonEmptyList Char)) -> capitalizeWord x == twice capitalizeWord x
      it "idem capitalize 2" $
        property $ \((NonEmpty x)::(NonEmptyList Char)) -> twice capitalizeWord x == fourTimes capitalizeWord x
      it "idem sort 1" $
        property $ \((NonEmpty x)::(NonEmptyList Char)) -> sort x == twice sort x
      it "idem sort 2" $
        property $ \((NonEmpty x)::(NonEmptyList Char)) -> twice sort x == fourTimes sort x

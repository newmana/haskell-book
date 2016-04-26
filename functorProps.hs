{-# LANGUAGE ViewPatterns #-}

import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Gen

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = (fmap id f) == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type IntPair = Pair Int -> IntToInt -> IntToInt -> Bool
type IntTwo = Two Int Int -> IntToInt -> IntToInt -> Bool
type IntThree = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type IntThree' = Three' Int Int -> IntToInt -> IntToInt -> Bool
type IntFour = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
type IntFour' = Four' Int Int -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Show, Eq)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)
data Trivial = Trivial deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor (Pair) where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- Needs at least one argument
-- instance Functor (Trivial) where
--   fmap f (Trivial) = Trivial

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  let li x = functorCompose (+1) (*2) (x :: [Int])
  quickCheck li
  quickCheck (functorCompose' :: IntFC)
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: IntPair)
  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: IntTwo)
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: IntThree)
  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck (functorCompose' :: IntThree')
  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck (functorCompose' :: IntFour)
  quickCheck $ \x -> functorIdentity (x :: Four' Int Int)
  quickCheck (functorCompose' :: IntFour')

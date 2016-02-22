import           Control.Monad
import           Data.Semigroup
import           Test.QuickCheck

monoidAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Show, Eq)
newtype Identity a = Identity a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
data Three a b c = Three a b c deriving (Show, Eq)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a1 b1) (Two a2 b2) = (Two (a1 <> a2) (b1 <> b2))

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a1 b1 c1) (Three a2 b2 c2) = (Three (a1 <> a2) (b1 <> b2) (c1 <> c2))

trivialSmush :: Trivial -> Trivial -> Trivial
trivialSmush = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

identitySmush :: (Semigroup a) => Identity a -> Identity a -> Identity a
identitySmush = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

main :: IO ()
main = do
  -- monoidAssoc trivialSmush
  -- quickCheck (monoidAssoc :: (Eq a, Show a) => Trivial -> Trivial -> Trivial -> Bool)
  -- quickCheck (monoidAssoc :: (Eq a, Show a, Semigroup a) => Identity a -> Identity a -> Identity a -> Bool)
  -- quickCheck (monoidAssoc :: (Eq a, Show a, Eq b, Show b, Semigroup a, Semigroup b) => Two a b -> Two a b -> Two a b -> Bool)
  -- quickCheck (monoidAssoc :: (Eq a, Show a, Eq b, Show b, Eq c, Show c) => Three a b c -> Three a b c -> Three a b c -> Bool)
  return ()

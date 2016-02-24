import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Poly

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) b = b
  mappend (First' a) _ = First' a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Only a]

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    elements [First' Nada, First' (Only a)]

firstMappend :: (Eq a) => First' a -> First' a -> First' a
firstMappend = mappend

main :: IO ()
main = do
  quickCheck (monoidAssoc :: First' A -> First' A -> First' A -> Bool)
  quickCheck (monoidLeftIdentity :: First' A -> Bool)
  quickCheck (monoidRightIdentity :: First' A -> Bool)

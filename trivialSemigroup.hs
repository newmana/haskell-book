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
  arbitrary = Identity <$> arbitrary

identitySmush :: (Semigroup a) => Identity a -> Identity a -> Identity a
identitySmush = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool deriving (Show, Eq)
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = (BoolConj True)
  (<>) _ _ = (BoolConj False)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = (BoolDisj False)
  (<>) _ _ = (BoolDisj True)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
  (<>) (Fst a) (Fst b) = Fst b
  (<>) (Fst a) (Snd b) = Snd b
  (<>) (Snd a) (Fst b) = Snd a
  (<>) (Snd a) (Snd b) = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

f = Combine $ \n -> Sum (n+1)
g = Combine $ \n -> Sum (n-1)

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (<>) f@(Combine a) g@(Combine b) = f

main :: IO ()
main = do
  quickCheck (monoidAssoc :: Trivial -> Trivial -> Trivial -> Bool)
--  quickCheck (monoidAssoc :: Identity Bool -> Identity Bool -> Identity Bool -> Bool)
--   quickCheck (monoidAssoc :: (Eq a, Show a, Eq b, Show b, Semigroup a, Semigroup b) => Two a b -> Two a b -> Two a b -> Bool)
--   quickCheck (monoidAssoc :: (Eq a, Show a, Eq b, Show b, Eq c, Show c, Semigroup a, Semigroup b, Semigroup c) => Three a b c -> Three a b c -> Three a b c -> Bool)
--   quickCheck (monoidAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
--   quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
--   quickCheck (monoidAssoc :: (Eq a, Show a, Eq b, Show b) => Or a b -> Or a b -> Or a b -> Bool)
  return ()

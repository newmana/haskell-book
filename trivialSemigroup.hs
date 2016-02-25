import           Control.Monad
import           Data.Semigroup
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, elements,
                                  quickCheck)

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
  (<>) (Snd a) _ = Snd a
  (<>) _ b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine a) (Combine b) = Combine $ \x -> (a x) <> (b x)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (<>) (Comp a) (Comp b) = Comp (a . b)

f' = Comp $ \n -> n + 1
g' = Comp $ \n -> n - 1

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure a) (Failure b) = Failure (a <> b)
  (<>) (Failure a) _ = Failure a
  (<>) _ (Failure b) = Failure b
  (<>) (Success a) _ = Success a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Success a), (Failure b)]

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success a)) (AccumulateRight (Success b)) = AccumulateRight (Success b)
  (<>) (AccumulateRight (Failure a)) _ = AccumulateRight (Failure a)
  (<>) _ (AccumulateRight (Failure b)) = AccumulateRight (Failure b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateRight (Success a), AccumulateRight (Failure b)]

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success a)) (AccumulateBoth (Success b)) = AccumulateBoth (Success (a <> b))
  (<>) (AccumulateBoth (Failure a)) _ = AccumulateBoth (Failure a)
  (<>) _ (AccumulateBoth (Failure b)) = AccumulateBoth (Failure b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateBoth (Success a), AccumulateBoth (Failure b)]

main :: IO ()
main = do
  quickCheck (monoidAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (monoidAssoc :: Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool)
  quickCheck (monoidAssoc :: Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Bool)
  quickCheck (monoidAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidAssoc :: Or String Int -> Or String Int -> Or String Int -> Bool)
  quickCheck (monoidAssoc :: Or String Int -> Or String Int -> Or String Int -> Bool)
  print ((unCombine (f <> g) $ 1) == Sum { getSum = 2 })
  print ((unCombine (g <> f) $ 1) == Sum { getSum = 2 })
  print ((unComp (f' <> g') $ 1) == 1)
  print ((unComp (g' <> f') $ 1) == 1)
  quickCheck (monoidAssoc :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)
  quickCheck (monoidAssoc :: AccumulateRight String [Int] -> AccumulateRight  String [Int] -> AccumulateRight  String [Int] -> Bool)
  quickCheck (monoidAssoc :: AccumulateBoth String [Int] -> AccumulateBoth  String [Int] -> AccumulateBoth  String [Int] -> Bool)
  return ()

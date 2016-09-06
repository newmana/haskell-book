import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation err a = Failure' err
   | Success' a deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure' err) = Left err
validToEither (Success' a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure' err
eitherToValid (Right a) = Success' a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id

data Errors = DividedByZero
 | StackOverflow
 | MooglesChewedWires deriving (Eq, Show)

data Sum a b = First a
  | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

instance Applicative (Sum a) where
  pure = Second
  (<*>) _ (First a) = First a
  (<*>) (First f) _ = First f
  (<*>) (Second f) (Second a) = Second (f a)

instance Functor (Validation e) where
  fmap _ (Failure' a) = Failure' a
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' a) (Failure' b) = Failure' (mappend a b)
  (<*>) (Failure' f) _ = Failure' f
  (<*>) _ (Failure' a) = Failure' a
  (<*>) (Success' f) (Success' a) = Success' (f a)

applyIfBothSecond :: (Sum e) (a -> b) -> (Sum e) a -> (Sum e) b
applyIfBothSecond a b = a <*> b

second = applyIfBothSecond (Second (+1)) (Second 123)
first = applyIfBothSecond (Second (+1)) (First 123)
ignoreFirst = applyIfBothSecond (First 345) (First 123)

instance Arbitrary a => Arbitrary (Sum a b) where
  arbitrary = First <$> arbitrary

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

testSum = quickBatch $ applicative (undefined :: Sum Int (Int, Int, Int))

success ::  (Num a, Monoid err) => Validation err a
success = Success' (+1) <*> Success' 1

failure :: Validation [Errors] Int
failure = Success' (+1) <*> Failure' [StackOverflow]

failures :: Validation [Errors] a
failures = Failure' [MooglesChewedWires] <*> Failure' [StackOverflow]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = Failure' <$> arbitrary

instance Arbitrary Errors where
  arbitrary = elements [DividedByZero, StackOverflow, MooglesChewedWires]

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

applyMappendError :: Monoid e => (Validation e) (a -> b) -> (Validation e) a -> (Validation e) b
applyMappendError a b = a <*> b

testValidation = quickBatch $ applicative (undefined :: Validation [Errors] (Int, Int, Int))

import           Control.Applicative      (liftA3)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

listPure = pure :: (a -> [] a)
listAp = (<*>) :: [] (a -> b) -> [] a -> [] b
listTest = listAp (listPure (+1)) (listPure 2)

ioPure = pure :: a -> IO a
ioAp = (<*>) :: IO (a -> b) -> IO a -> IO b
ioTest = ioAp (ioPure (+1)) (ioPure 2)

tuplePure :: Monoid b => a -> (b, a)
tuplePure = pure :: Monoid b => a -> ((,) b) a

tupleAp :: Monoid c => (c, a -> b) -> (c, a) -> (c, b)
tupleAp = (<*>) :: Monoid c => (,) c (a -> b) -> (,) c a -> (,) c b

tupleTest = tupleAp (tuplePure (+1) :: ([Int], Int -> Int)) (tuplePure 2 :: ([Int], Int))
tupleTest2 = tupleAp (tuplePure (+1) :: (String, Int -> Int)) (tuplePure 2 :: (String, Int))

arrowPure = pure :: (a -> ((->) e) a)
arrowAp = (<*>) :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b
arrowTest = arrowAp (arrowPure (+1)) (arrowPure 2) id

newtype Identity a = Identity a deriving (Show, Eq)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a2) (f2 a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a1 f1) (Two a2 f2) = Two (a1 <> a2) (f1 f2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a1 b1 c1) (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' a1 f1 g1) (Three' a2 f2 g2) = Three' (a1 <> a2) (f1 f2) (g1 g2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a1 b1 c1 d1) (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a1 b1 c1 d1) (Four' a2 b2 c2 d2) = Four' (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 d2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main = do
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))
  quickBatch $ applicative (undefined :: Two [Int] (Int, Int, Int))
  quickBatch $ applicative (undefined :: Three [Int] [Int] (Int, Int, Int))
  quickBatch $ applicative (undefined :: Three' [Int] (Int, Int, Int))
  quickBatch $ applicative (undefined :: Four [Int] [Int] [Int] (Int, Int, Int))
  quickBatch $ applicative (undefined :: Four' [Int] (Int, Int, Int))

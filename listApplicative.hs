import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' i (Cons x xs) = Cons x (take' (i - 1) xs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Applicative List where
  pure a = Cons a Nil
  (<*>) f a = flatMap (\g -> fold (Cons . g) Nil a) f

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

foreverList :: a -> List a
foreverList x = Cons x (foreverList x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ _ Nil = Nil
zipWith' _ Nil _ = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure = ZipList' . foreverList
  (<*>) (ZipList' fs) (ZipList' as) = ZipList' $ zipWith' id fs as

a = Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
b = flatMap (\x -> Cons 1 (Cons x Nil) ) a

main = quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))

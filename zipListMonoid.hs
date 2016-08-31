import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

a :: Sum Integer
a = 1 <> 2


instance Monoid a => Monoid (ZipList a) where
  -- mempty = ZipList []
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

main =  quickBatch $ monoid (ZipList [1 :: Sum Int])

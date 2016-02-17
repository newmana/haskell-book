import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Monoid a, Num a) => Monoid (Optional a) where
  mempty = Nada
  mappend (Nada) (Only a) = Only a
  mappend (Only a) (Nada) = Only a
  mappend (Only a) (Only b) = Only (a + b)

test1 = Only (Sum 1) `mappend` Only (Sum 1) == Only (Sum 2)
test2 = Only (Sum 1) `mappend` Nada == Only (Sum 1)
test3 = Nada `mappend` Only (Sum 1) == Only (Sum 1)
allTests = (test1 == True) && (test2 == True) && (test3 == True)
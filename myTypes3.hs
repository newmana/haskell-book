{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool
  
instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
  tooMany (n1, n2) = n1 + n2 > 42

instance (Ord a, Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = n1 + n2 > 42
    
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

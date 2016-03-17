-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

class Sumthin a where
  s :: a -> a

class Else f where
  e :: b -> f (g a b c)

class Biffy e where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

-- class Impish v where
--   impossibleKind :: v -> v a

class NotSoBad b where
  notSoBad :: a -> b a -> b a

class IsItBad c where
  bad :: c a b -> c b a

-- data FixMePls = FixMe | Pls deriving (Eq, Show)

-- instance Functor FixMePls where
--   fmap = error "it doesn't matter, it won't compile"

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)
instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  -- fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

data CountingBad a = Heisenberg Int a deriving (Eq, Show)
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

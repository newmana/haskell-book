{-# LANGUAGE FlexibleInstances #-}

-- should have kind * -> *
-- data Bool1 = False1 | True1

-- instance Functor (Bool1) where
--   fmap _ (True1) = True1
--   fmap _ (False1) = False1

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

data BoolAndSomethingElse a =
  False' a | True' a deriving (Show, Eq)

instance Functor (BoolAndSomethingElse) where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a deriving (Show, Eq)

instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

data Sum a b = First a | Second b deriving (Show, Eq)

instance Functor (Sum e) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c = DeepBlue a c | Something b deriving (Show, Eq)

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst (f a)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut (f <$> a)

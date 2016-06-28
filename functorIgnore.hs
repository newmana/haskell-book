{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+ 1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap f LolNope = LolNope

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+ 1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (First b) = First b
  fmap f (Second a) = Second (f a)


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")

andThat :: IO String
andThat = fmap (++ " yeah and that") getLine

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

type Nat' f g a = f a -> g a

maybeToList' :: Nat' Maybe [] a
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap (+1) (Flip (Tuple 1 "blah")) fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

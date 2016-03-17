import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, elements,
                                  quickCheck)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

data Trivial = Trivial deriving (Show, Eq)
newtype Identity a = Identity a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  (mappend) (Identity a) (Identity b) = Identity (a <> b)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  (mappend) (Two a1 b1) (Two a2 b2) = (Two (a1 `mappend` a2) (b1 `mappend` b2))

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool deriving (Show, Eq)
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Monoid BoolConj where
  mempty = (BoolConj True)
  (mappend) (BoolConj True) (BoolConj True) = (BoolConj True)
  (mappend) _ _ = (BoolConj False)

instance Monoid BoolDisj where
  mempty = (BoolDisj False)
  (mappend) (BoolDisj False) (BoolDisj False) = (BoolDisj False)
  (mappend) _ _ = (BoolDisj True)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

data Or a b = Fst a | Snd b | Empty deriving (Show, Eq)

instance Monoid (Or a b) where
  mempty = Empty
  (mappend) (Snd a) _ = Snd a
  (mappend) a Empty = a
  (mappend) _ b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  (mappend) (Combine a) (Combine b) = Combine $ mappend a b

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Monoid (Comp a) where
  mempty = Comp id
  (mappend) (Comp a) (Comp b) = Comp (a . b)

f' = Comp $ \n -> n + 1
g' = Comp $ \n -> n - 1

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) = Mem $ \s -> (fst (f s) <> fst (g s), snd $ g $ snd (f s))

f'' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc :: Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool)
  quickCheck (monoidLeftIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (monoidAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidAssoc :: Or String [Int] -> Or String [Int] -> Or String [Int] -> Bool)
  quickCheck (monoidLeftIdentity :: Or String [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Or String [Int] -> Bool)
  print ((unCombine (f <> g) $ 1) == Sum { getSum = 2 })
  print ((unCombine (g <> f) $ 1) == Sum { getSum = 2 })
  print ((unComp (f' <> g') $ 1) == 1)
  print ((unComp (g' <> f') $ 1) == 1)
  print $ runMem (f'' <> mempty) 0
  print $ runMem (mempty <> f'') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f'' <> mempty) 0 == runMem f'' 0
  print $ runMem (mempty <> f'') 0 == runMem f'' 0
  return ()

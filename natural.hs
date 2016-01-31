import Data.Maybe

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat 
integerToNat 0 = Just Zero
integerToNat x = if x < 0 then Nothing else Just(unpackMaybe x)
    where unpackMaybe x = Succ(fromMaybe Zero (integerToNat (x-1)))

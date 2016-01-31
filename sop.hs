data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                  , psecond :: b }
  deriving (Eq, Show)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

--data Twitter =
--  Twitter deriving (Eq, Show)

--data AskFm =
--  AskFm deriving (Eq, Show)
type Twitter = String
type AskFm = String
data SocialNetwork = Twitter | AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First "Twitter"

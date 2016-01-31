import Control.Applicative

type Name = String
type Age = Integer
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)
data Person = Person Name Age deriving Show 
type ValidatePerson a = Either [PersonInvalid] a


data Validation err a = Failure err | Success a deriving (Eq, Show)

ageOkay :: Age -> Validation [PersonInvalid] Age 
ageOkay age = case age > 0 of
  True -> Failure age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Validation [PersonInvalid] Name 
nameOkay name = case name /= "" of
  True -> Right name 
  False -> Left [NameEmpty]

--mkPerson :: Name -> Age -> ValidatePerson Person 
-- mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)
mkPerson :: Name -> Age -> Validation [PersonInvalid] Person 
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age) 


mkPerson' :: ValidatePerson Name 
          -> ValidatePerson Age 
          -> ValidatePerson Person

-- Happy Path
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
-- Both Bad
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
-- One Bad
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
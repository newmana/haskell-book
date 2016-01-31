data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Size = Small | Medium | Jumbo deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Size Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane Jumbo PapuAir

isCar :: Vehicle -> Bool
isCar (Car m p) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane s a) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar 

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m
getManu _ = error "No manufacturer" 

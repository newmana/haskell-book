import Data.Time
data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
   , DbString "Hello, world!"
   , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr extract []
  where
    extract (DbDate a) acc = a : acc
    extract _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr extract []
  where
    extract (DbNumber a) acc = a : acc
    extract _ acc = acc 

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max maxDate . filterDbDate 
  where
    maxDate = (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = avg
  where
    fracSubDb = realToFrac . sumDb
    intLength = fromIntegral . length
    avg a = (fracSubDb a) / (intLength a)

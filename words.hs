import Data.List
import Data.Char

notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe a = Just a  

replaceThe :: String -> String
replaceThe a =
  case temp of
    (' ':xs) -> xs
    otherwise -> temp
  where temp = stripMaybe $ map notThe $ words a

stripMaybe :: [Maybe String] -> String
stripMaybe [] = ""
stripMaybe (Nothing:xs) = stripMaybe xs
stripMaybe (Just w:xs) = ' ' : w ++ stripMaybe xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel a = go 0 $ words a
  where 
    go a [] = a
    go a [x] = a
    go a (x1:x2:xs) = if (x1 == "the") && not (null ("aeuio" `intersect` [head x2])) 
      then go (a+1) xs 
      else go a xs
      
countTheBeforeVowel' :: String -> Integer
countTheBeforeVowel' w =
  foldr (\(x1,x2) a -> if (x1 == "the") && not (null ("aeuio" `intersect` [head x2])) then a + 1 else a ) 0 (blah w)
    where blah w = zip (a w) (b w)
          a = words
          b w = tail $ a w

countVowels :: String -> Integer 
countVowels = toInteger . length . whereHood vowelHood
    
newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
consonants = "bcdfghjklmpqrstvwxyz"

vowelHood a = vowels `intersect` [toLower a]
consonantHood a = consonants `intersect` [toLower a]

whereHood hood = findIndices (not . null . hood)    

mkWord :: String -> Maybe Word'
mkWord x = 
  if length (whereHood vowelHood x) > length (whereHood consonantHood x)
    then Nothing
    else Just (Word' x)

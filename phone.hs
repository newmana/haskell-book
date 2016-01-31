import Data.List
import Data.Char
import Data.Map

type Symbol = String
data DaPhone = Phone [(Symbol, Digit)]

convo :: [String]
convo = ["Wanna play 20 questions",
              "Ya",
              "U 1st haha",
              "Lol ok. Have u ever tasted alcohol lol",
              "Lol ya",
              "Wow ur cool haha. Ur turn",
              "Ok. Do u think I am pretty Lol",
              "Lol ya",
              "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

phone = Phone[("1", '1'), ("ABC2", '2'), ("DEF3", '3'), ("GHI4", '4'), ("JKL5", '5'), 
  ("MNO6", '6'), ("PQRS7", '7'), ("TUV8", '8'), ("WXYZ9", '9'), ("+ ", '0'),
  (".,", '#'), ("^", '*') ] 

isAnUpper :: Char -> [Digit]
isAnUpper c = ['*' | isUpper c] 

findDigit :: DaPhone -> Char -> [Digit]
findDigit (Phone phone) c = Data.List.foldr (\(x, y) acc -> if toUpper c `elem` x then (takeWhile (/= toUpper c) x) ++ [toUpper c] ++ acc else acc) [] phone
  
combineDigits :: DaPhone -> Char -> [Digit]
combineDigits p c = findDigit p c ++ isAnUpper c

findDigit' :: DaPhone -> String -> [Digit]
findDigit' p = Data.List.foldr (\e a -> a ++ combineDigits phone e) []

hist :: (Ord a, Num b) => [a] -> Map a b
hist = Data.Map.fromListWith (+) . (`zip` repeat 1)
appendSentences = Data.List.foldr (\e a -> e ++ " " ++ a) ""
maxTuple = Data.List.foldr1 (\e a -> if snd e > snd a then e else a) 

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)] 
cellPhonesDead p s = Data.Map.toList $ hist $ findDigit' p s 

fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps = Data.List.foldr (\(d, p) a -> a + p) 0

popularest :: String -> Char
popularest s = fst $ maxTuple $ toList $ hist $ concat $ words s

reverseTaps :: Char -> (Digit, Presses) 
reverseTaps c = (c, length $ combineDigits phone c)

coolestLtr :: [String] -> Char 
coolestLtr s = fst $ maxTuple $ toList $ hist $ concat $ words $ appendSentences s

coolestWord :: [String] -> String
coolestWord s = fst $ maxTuple $ toList $ hist $ words $ Data.List.filter (/= '.') $ appendSentences s
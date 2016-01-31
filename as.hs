import Data.Char
import Control.Applicative
import Data.List (intercalate, unfoldr)

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf x@(a:as) y@(b:bs) = if a == b then isSubsequenceOf as bs else isSubsequenceOf x bs
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capitalizeWord x)) . words
    
capitalizeWord :: String -> String
capitalizeWord = liftA2 (:) (toUpper . head) tail

capitalizeParagraph :: String -> String 
capitalizeParagraph a = intercalate "." (map blurb $ split '.' a) ++ f a
  where
    f a = if last a == '.' then "." else ""

blurb x@(' ':xs) = " " ++ capitalizeWord xs
blurb x = capitalizeWord x

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) 
  where 
    (x,y) = span (/= d) s
module Strings where

takeLast :: String -> String
takeLast s = reverse (drop 1 (reverse s))

takeFour :: [Char] -> Char
takeFour s = s !! 4

thirdLetter :: [Char] -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char 
letterIndex x = aweCurry !! x

dropNine :: String -> String
dropNine s = drop 9 s

reverseWords :: String -> String
reverseWords s = drop 9 s ++ " " ++ (take 2 (drop 6 s)) ++ " " ++ (take 5 s)

aweCurry :: String
aweCurry = "Curry is awesome!"

main :: IO () 
main = do
  putStrLn (takeLast aweCurry)
  putStrLn (show (takeFour aweCurry))
  putStrLn (dropNine aweCurry)

import Data.List

problem55 = length $ filter (==True) $ map (isLychrel 50) input

isLychrel 1 n = True
isLychrel i n = if isPalindrome sum then False else isLychrel (i - 1) sum 
  where sum = n + reverse' n

isPalindrome n = n == reverse' n 

reverse' = fromDigits . reverse . toDigits

fromDigits = foldl (\acc x -> 10 * acc + x) 0

toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

input = [1..9999]
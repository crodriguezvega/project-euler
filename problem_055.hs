import Data.List

problem55 :: Int
problem55 = length $ filter (==True) $ map (isLychrel 50) input

isLychrel :: (Eq t, Num t) => t -> Integer -> Bool
isLychrel 1 n = True
isLychrel i n = if isPalindrome sum then False else isLychrel (i - 1) sum 
  where sum = n + reverse' n

isPalindrome :: Integer -> Bool
isPalindrome n = n == reverse' n 

reverse' :: Integer -> Integer
reverse' = fromDigits . reverse . toDigits

fromDigits :: [Integer] -> Integer
fromDigits = foldl (\acc x -> 10 * acc + x) 0

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

input :: [Integer]
input = [1..9999]
import Data.List

problem52 :: Integer
problem52 = head $ dropWhile predicate candidates

predicate :: Integral a => a -> Bool
predicate n = not $ condition n (products n)

condition :: Integral t => t -> [t] -> Bool
condition n [] = True
condition n (x:xs) = sameDigits n x && condition n xs

sameDigits :: Integral a => a -> a -> Bool
sameDigits n m = length a == length b && (a \\ b) == []
  where a = toDigits n
        b = toDigits m

products :: (Num a, Enum a) => a -> [a]
products n = [n * x | x <- [1..6]]

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

candidates :: [Integer]
candidates = [y | x <- [0..], let a = 10 * 10 ^ x, let b = 16 * 10 ^ x, y <- [a..b]]
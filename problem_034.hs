factorial :: Int -> Int
factorial n = product [1..n]

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

sumFactorialOfDigits :: Int -> Int
sumFactorialOfDigits n = sum $ map factorial $ digits n

problem34 :: Int
problem34  = sum [x | x <- [3..100000], x == sumFactorialOfDigits x]
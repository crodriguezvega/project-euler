problem20 :: Integer
problem20 = sum $ digits $ product [1..100]

digits :: Integer -> [Integer]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)
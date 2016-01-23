import Data.Bits

problem16 :: Integer
problem16 = sum $ digits $ 2 `shift` (1000 - 1)

digits :: Integer -> [Integer]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)
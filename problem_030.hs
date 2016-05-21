problem30 :: Int
problem30 = sum [n | n <- numbers, isEqualToSumOfFifthPowers n] 

isEqualToSumOfFifthPowers n = n == sumFifthPowers
  where sumFifthPowers = sum fifthPowers  
        fifthPowers = map fifthPower digits
        digits = toDigits n

toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

numbers = let upperBound = 355000 in [2..upperBound]

fifthPower 0 = 0
fifthPower 1 = 1
fifthPower 2 = 32
fifthPower 3 = 243
fifthPower 4 = 1024
fifthPower 5 = 3125
fifthPower 6 = 7776
fifthPower 7 = 16807
fifthPower 8 = 32768
fifthPower 9 = 59049
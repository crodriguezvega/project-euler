problem48 :: Integer
problem48 = sumSelfPowers `mod` mask

limit :: Integer
limit = 1000

mask :: Integer
mask = 10000000000

sumSelfPowers :: Integer
sumSelfPowers = sum $ map (\x -> x ^ x) [1..limit]
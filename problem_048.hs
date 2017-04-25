problem48 = sumSelfPowers `mod` mask

limit = 1000
mask = 10000000000

sumSelfPowers = sum $ map (\x -> x ^ x) [1..limit]
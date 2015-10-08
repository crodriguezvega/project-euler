sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (1 : tail) = sieve [2..]
sieve (head : tail) = head : sieve [x | x <- tail, x `mod` head > 0]

problem7 :: Integer
problem7 =  primes !! 10001 where primes = sieve [2..]
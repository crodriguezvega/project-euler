primes :: [Int]
primes = 2 : sieve [3,5..]
  where sieve [] = []
        sieve (head : tail) = head : sieve [x | x <- tail, x `mod` head > 0]

problem7 :: Int
problem7 =  primes !! 10001
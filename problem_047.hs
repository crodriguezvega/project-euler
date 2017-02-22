{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell

  Smallest number with four different prime factors is 210 = 2 * 3 * 5 * 7
-}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem47 = head $ head
                 $ dropWhile (\x -> not (all has4DistinctPrimes x))
                 $ map groupOf4 [210..]

groupOf4 start = take 4 [start..]

has4DistinctPrimes n = length uniqueFactors == 4
  where factors = primeFactors n
        uniqueFactors = nub factors

-- http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
primeFactors n =
  case factors of
  [] -> [n]
  _  -> factors ++ primeFactors (n `div` (head factors))
  where squareRoot = floor . sqrt . fromIntegral
        primes = takeWhile (<= squareRoot n) primesSAE
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) primes

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
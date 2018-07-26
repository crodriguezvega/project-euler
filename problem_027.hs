{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
  
  - For n = 0: n^2 + a*n + b = b, then b must be prime.
  - For n = 1: 1 + a + b must be prime, and since all primes except 2 are odd, then
    a + b must be even and thus both a and b are odd.
-}

import Data.List
import Data.Ord
import Data.List (inits)
import Data.Array.Unboxed

problem27 :: Int
problem27 = fst $ maximumBy (comparing snd) $ quadraticPrimes coefficients
  where coefficients = [(i, j) | i <- a, j <- b]
        a = toNegative oddNumbers ++ oddNumbers
        b = toNegative primes ++ primes
        oddNumbers = [1, 3 .. 999]

toNegative :: [Int] -> [Int]
toNegative = map (*(-1))

primes :: [Int]
primes = takeWhile (<1000) primesSAE

isPrime :: Int -> Bool
isPrime n = elem n primes

quadraticFormula :: Int -> Int -> Int -> Int
quadraticFormula n a b = n ^ 2 + n * a + b

quadraticPrimes :: [(Int, Int)] -> [(Int, Int)]
quadraticPrimes []          = []
quadraticPrimes ((a, b):xs) = (a * b, numberOfPrimes) : quadraticPrimes xs
  where numberOfPrimes = length $ takeWhile (\n -> isPrime $ quadraticFormula n a b) [0..]

primesSAE :: [Int]
primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft

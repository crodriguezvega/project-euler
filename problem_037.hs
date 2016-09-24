{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell

  From wikipedia we know that primes that are both left and right truncatable are smaller that 1 million:
  https://en.wikipedia.org/wiki/Truncatable_prime
-}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem37 = sum $ take 11 $ truncatablePrimes [3, 7]

truncatablePrimes [] = [] 
truncatablePrimes (x:xs) = truncatable ++ truncatablePrimes list
  where candidates = appendDigits x
        list = filter isPrime (xs ++ candidates)
        truncatable = filter isTruncatable candidates

isTruncatable p = isLeftTruncatable p && isRightTruncatable p
isLeftTruncatable = isPrime
isRightTruncatable p
  | quotient == 0 = True
  | otherwise     = (isPrime quotient) && isRightTruncatable quotient
  where quotient = removeDigit p

appendDigits digit = map (\x -> appendDigit x digit) [1, 2, 3, 5, 7, 9]
removeDigit number = quot number 10
appendDigit digit number = digit * (10 ^ power) + number
  where power = floor (logBase 10 $ fromIntegral number) + 1

isPrime number = elem number primes 
primes = takeWhile (<800000) primesSAE

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
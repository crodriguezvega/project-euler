{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell

  From wikipedia we know that primes that are both left and right truncatable are smaller that 1 million:
  https://en.wikipedia.org/wiki/Truncatable_prime
-}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem37 :: Int
problem37 = sum $ take 11 $ truncatablePrimes [3, 7]

truncatablePrimes :: [Int] -> [Int]
truncatablePrimes [] = [] 
truncatablePrimes (x:xs) = truncatable ++ truncatablePrimes list
  where candidates = appendDigits x
        list = filter isPrime (xs ++ candidates)
        truncatable = filter isTruncatable candidates

isTruncatable :: Int -> Bool
isTruncatable p = isLeftTruncatable p && isRightTruncatable p

isLeftTruncatable :: Int -> Bool
isLeftTruncatable = isPrime

isRightTruncatable :: Int -> Bool
isRightTruncatable p
  | quotient == 0 = True
  | otherwise     = (isPrime quotient) && isRightTruncatable quotient
  where quotient = removeDigit p

appendDigits :: Integral b => b -> [b]
appendDigits digit = map (\x -> appendDigit x digit) [1, 2, 3, 5, 7, 9]

removeDigit :: Integral a => a -> a
removeDigit number = quot number 10

appendDigit :: Integral a => a -> a -> a
appendDigit digit number = digit * (10 ^ power) + number
  where power = floor (logBase 10 $ fromIntegral number) + 1

isPrime :: Int -> Bool
isPrime number = elem number primes 

primes :: [Int]
primes = takeWhile (<800000) primesSAE

primesSAE :: [Int]
primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
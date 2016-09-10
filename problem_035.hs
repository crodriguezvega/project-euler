{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
-}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem35 :: Int
problem35 = length $ circularPrimes primes

circularPrimes [] = []
circularPrimes p@(x:xs)
  | sumDigits > 2 && isMultipleOf2 digits    = circularPrimes xs
  | sumDigits > 3 && isMultipleOf3 sumDigits = circularPrimes xs
  | sumDigits > 5 && isMultipleOf5 digits    = circularPrimes xs
  | all (\x -> elem x p) rotations           = rotations ++ circularPrimes remaining
  | otherwise                                = circularPrimes xs
  where digits    = reverse $ toDigits x
        sumDigits = sum digits
        rotations = nub $ map fromDigits $ rotate $ digits
        remaining = filter (\x -> not $ elem x rotations) xs

isMultipleOf2 digits = any (\x -> x `mod` 2 == 0) digits
isMultipleOf3 sumDigits = sumDigits `mod` 3 == 0
isMultipleOf5 digits = elem 0 digits || elem 5 digits

-- http://stackoverflow.com/questions/7631664/how-to-define-a-rotates-function
shift [] = []
shift (x:xs) = xs ++ [x]

rotate l = take (length l) (iterate shift l)

fromDigits :: Num a => [a] -> a
fromDigits = foldl (\acc x -> 10 * acc + x) 0

toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

primes = takeWhile (<1000000) primesSAE

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
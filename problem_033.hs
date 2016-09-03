{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
-}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed
import qualified Data.Map as Map

type IntMap = Map.Map Int Int

problem33 :: Int
problem33 = quot denominatorProduct greatestCommonDivisor
  where solutions             = filter isNonTrivial fractions
        numeratorProduct      = product $ map fst solutions 
        denominatorProduct    = product $ map snd solutions
        greatestCommonDivisor = (gcd numeratorProduct denominatorProduct)

isNonTrivial fraction
  | numerator `mod` 10 == 0 || denominator `mod` 10 == 0 = False
  | (length singleNumerator) > 1 || (length singleDenominator) > 1 = False
  | otherwise = any (\f -> reduced == f) $ map reduceFraction possibleFractions
  where reduced            = reduceFraction fraction
        numerator          = fst fraction
        denominator        = snd fraction
        numeratorDigits    = toDigits numerator
        denominatorDigits  = toDigits denominator
        singleNumerator    = numeratorDigits \\ denominatorDigits
        singleDenominator  = denominatorDigits \\ numeratorDigits
        possibleFractions  = [(num, den) | num <- singleNumerator, den <- singleDenominator, den > num]

reduceFraction fraction = (reducedNumerator, reducedDenominator)
  where numerator          = fst fraction
        denominator        = snd fraction
        numeratorFactors   = factors Map.! numerator
        denominatorFactors = factors Map.! denominator
        reducedNumerator   = product (numeratorFactors \\ denominatorFactors)
        reducedDenominator = product (denominatorFactors \\ numeratorFactors)

fractions :: [(Int, Int)]
fractions = [(num, den) | num <- [10..99], den <- [10..99], den > num]

factors = foldl (\m n -> Map.insert n (factorization n primes) m) Map.empty [1..99]

factorization number primes@(p:ps)
  | number == 1         = []
  | number `mod` p == 0 = p : factorization quotient primes
  | otherwise           = factorization number ps
  where quotient = number `div` p

toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

primes = takeWhile (<100) primesSAE

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
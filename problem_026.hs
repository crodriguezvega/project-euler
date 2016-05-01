{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
  
  From:
  https://en.wikipedia.org/wiki/Repeating_decimal
  "A fraction in lowest terms with a prime denominator other than 2 or 5 (i.e. coprime to 10) always
  produces a repeating decimal. The length of the repetend (period of the repeating decimal) of 1/p is
  equal to the order of 10 modulo p."
-}

import Data.Ord
import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem26 :: Integer
problem26 = fst $ maximumBy (comparing snd)
                $ map (\p -> (p, multiplicativeOrder 10 p))
                $ map toInteger primes
            where primes = takeWhile (<1000) primesSAE

multiplicativeOrder a 2 = 0
multiplicativeOrder a 5 = 0
multiplicativeOrder a n = fst $ head
                              $ dropWhile (\x -> mod (snd x) n /= 0) [(k, a^k - 1) | k <- [1..n]]

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
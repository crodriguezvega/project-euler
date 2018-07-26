{--
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell

  Formula to generate odd composite numbers:
  http://oeis.org/wiki/Odd_composites
--}

import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem46 :: Int
problem46 = head $ dropWhile isSumOfPrimeAndTwiceSquare oddComposites

oddComposites :: [Int]
oddComposites = [(2 * i + 1) * (2 * j + 1) | i <- [1..], j <- [1..i]]

isSumOfPrimeAndTwiceSquare :: Int -> Bool
isSumOfPrimeAndTwiceSquare n = any isTwiceSquare $ map (\x -> fromIntegral (n - x)) primes
  where primes = takeWhile (<n) primesSAE

isTwiceSquare :: (RealFrac a, Floating a) => a -> Bool
isTwiceSquare n = isInt $ sqrt (n / 2)
isInt n = n == fromInteger (round n)

primesSAE :: [Int]
primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
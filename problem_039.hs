{-
  We need to generate first the list of primitive Pythagorean triplets:

  http://mathforum.org/library/drmath/view/55811.html
  http://aleph0.clarku.edu/~djoyce/numbers/pyth.pdf

  Thus the formulas that give all Pythagorean triples are these:

   a = d*(m^2 - n^2),
   b = 2*d*m*n,
   c = d*(m^2 + n^2),

  where d is any positive integer, m > n > 0 are integers of opposite 
  parity and relatively prime.
-}

import Data.Ord
import Data.List

problem39 :: Integer
problem39 = head $ maximumBy (comparing (\x -> length x)) 
                 $ group
                 $ sort
                 $ map sumTriplet
                 $ concatMap generateMultipleTriplets
                 $ concat
                 $ takeWhile (\x -> length x > 0)
                 $ map generatePrimitiveTriplets [2..]

generateMultipleTriplets :: (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]                 
generateMultipleTriplets primitive = takeWhile condition $ map generateMultiple [1..]
  where generateMultiple d = multiplePythagoreanTriplet primitive d

generatePrimitiveTriplets :: Integer -> [(Integer, Integer, Integer)]
generatePrimitiveTriplets m = takeWhile condition $ map primitivePythagoreanTriplet pairs
  where pairs = [(m, n) | n <- generateN m]

generateN :: Integral a => a -> [a]
generateN m = filter (\x -> isCoprime x && isOppositeParity x) [start..m-1]
  where isOppositeParity n = not $ isEven $ abs (m - n)
        isCoprime n = gcd n m == 1
        isEven x = x `mod` 2 == 0
        start | isEven m  = 1
              | otherwise = 2

multiplePythagoreanTriplet :: Num a => (a, a, a) -> a -> (a, a, a)
multiplePythagoreanTriplet (a, b, c) d = (d * a, d * b, d * c)

primitivePythagoreanTriplet :: Num a => (a, a) -> (a, a, a)
primitivePythagoreanTriplet (m, n) =
  let a = m^2 - n^2
      b = 2 * m * n;
      c = m^2 + n^2;
  in (a, b, c)

sumTriplet :: Num a => (a, a, a) -> a
sumTriplet (a, b, c) = a + b + c

targetSum :: Integer
targetSum = 1000

condition :: (Integer, Integer, Integer) -> Bool
condition triplet = sumTriplet triplet <= targetSum
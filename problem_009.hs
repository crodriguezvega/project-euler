{-
  Using Euclid's formula to generate Pythagorean triplets:
  https://en.wikipedia.org/wiki/Pythagorean_triple
  
  Solution takes advantage of lazy evaluation, and that's why an infinite
  list of pairs of values (m, n) (following naming convention of Euclid's
  formula) is generated.
-}

problem9 :: Int
problem9 = 
  let targetSum = 1000
      pairs = [(m, n) | m <- [2..], n <- [1..m - 1]]
      condition triplet = sumTriplet triplet /= targetSum 
  in productTriplet $ head $ dropWhile condition $ map pythagoreanTriplet pairs
  
pythagoreanTriplet :: (Integral a) => (a, a) -> (a, a, a)
pythagoreanTriplet (m, n) =
  let a = m^2 - n^2
      b = 2 * m * n;
      c = m^2 + n^2;
  in (a, b, c)

productTriplet :: (Integral a) => (a, a, a) -> a
productTriplet (a, b, c) = a * b * c

sumTriplet :: (Integral a) => (a, a, a) -> a
sumTriplet (a, b, c) = a + b + c
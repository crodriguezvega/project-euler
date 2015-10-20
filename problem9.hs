sumTriplet :: (Integral a) => (a, a, a) -> a
sumTriplet (a, b, c) = a + b + c

productTriplet :: (Integral a) => (a, a, a) -> a
productTriplet (a, b, c) = a * b * c

pythagoreanTriplet :: (Integral a) => (a, a) -> (a, a, a)
pythagoreanTriplet (m, n) =
  let a = m^2 - n^2
      b = (2 * m * n);
      c = (m^2 + n^2);
  in (a, b, c)

problem9 :: Int
problem9 = 
  let targetSum = 1000
      pairs = [(x, y) | x <- [2..], y <- [1..x - 1]]
      condition triplet = sumTriplet triplet /= targetSum 
  in productTriplet $ head $ dropWhile condition $ map pythagoreanTriplet pairs

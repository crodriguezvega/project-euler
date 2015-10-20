sumTriplet :: (Integer, Integer, Integer) -> Integer
sumTriplet (a, b, c) = a + b + c

productTriplet :: (Integer, Integer, Integer) -> Integer
productTriplet (a, b, c) = a * b * c

pythagoreanTriplet :: (Integer, Integer) -> (Integer, Integer, Integer)
pythagoreanTriplet (m, n) =
  let a = m^2 - n^2
      b = (2 * m * n);
      c = (m^2 + n^2);
  in (a, b, c)

problem9 :: Integer
problem9 = 
  let targetSum = 1000
      pairs = [(x, y) | x <- [1..], y <- [1..x - 1]]
      condition triplet = sumTriplet triplet /= targetSum 
  in productTriplet $ head $ dropWhile condition $ map pythagoreanTriplet pairs
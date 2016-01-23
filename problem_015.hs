problem15 :: Integer
problem15 = binomialCoefficient 40 20

binomialCoefficient :: Integer -> Integer -> Integer
binomialCoefficient 0 _ = 1
binomialCoefficient _ 0 = 1
binomialCoefficient n k = numerator `div` denominator
                          where numerator = product [n - k + 1..n]
                                denominator = product [1..k]
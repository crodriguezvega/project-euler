problem3 :: Integer
problem3 = largestPrimeFactor 600851475143 2
  where largestPrimeFactor n f | n <= f    = f
                               | otherwise = largestPrimeFactor number factor
                                 where isDivisible = n `mod` f == 0 
                                       number = if isDivisible then n `div` f else n
                                       factor = if isDivisible then 2 else f + 1 
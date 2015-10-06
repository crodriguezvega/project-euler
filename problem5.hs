problem5 :: Integer
problem5 =
  let evenlyDivisible = [x | x <- [1..232792565], isEvenlyDivisible x]
  in head evenlyDivisible
  where isEvenlyDivisible n = foldl (\acc x -> n `mod` x == 0 && acc) True [1..20]
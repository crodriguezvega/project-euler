problem1 :: Integer
problem1 =
  let multiples = [n | n <- [1..999], n `mod` 3 == 0 || n `mod` 5 == 0]
  in sum multiples
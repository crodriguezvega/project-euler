problem6 :: Int
problem6 =
  let n = 100 in floor $ squareOfSum n - sumOfSquares n
  where squareOfSum n = (n * (n + 1) / 2) ^ 2
        sumOfSquares n = n * (n + 1) * (2 * n + 1) / 6
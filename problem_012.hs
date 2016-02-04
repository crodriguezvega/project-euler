{-
  Using the formula to generate triangular numbers:
  https://en.wikipedia.org/wiki/Triangular_number
  
  And the fact that divisors can be listed in pairs if we test
  for divisibility up to the square root of the number:
  http://mathschallenge.net/library/number/number_of_divisors
-}

import Data.List

problem12 :: Int
problem12 = head $ reverse
                 $ sort
                 $ head
                 $ dropWhile (\divisors -> length divisors < 500) 
                 $ map divisors [(x, [1..isqrt x]) | x <- triangularNumbers]

divisors :: (Int, [Int]) -> [Int]
divisors (n, [])     = []
divisors (n, (x:xs)) = case n `mod` x of 0 -> if x == y then x : divisors (n, xs)
                                              else x : y : divisors (n, xs)
                                                where y = quot n x
                                         _ -> divisors (n, xs)

triangularNumbers :: [Int]
triangularNumbers = [quot (x * (x + 1)) 2 | x <- [1..]]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
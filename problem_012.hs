import Data.List

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

triangularNumbers :: [Int]
triangularNumbers = [quot (x * (x + 1)) 2 | x <- [1..]]

divisors :: (Int, [Int]) -> [Int]
divisors (n, [])     = []
divisors (n, (x:xs)) = case n `mod` x of 0 -> x : y : divisors (n, xs)
                                              where y = quot n x
                                         _ -> divisors (n, xs)
                    
problem12 :: Int
problem12 = head $ reverse
                 $ sort
                 $ head
                 $ dropWhile (\divisors -> length divisors < 500) 
                 $ map divisors [(x, [1..isqrt x]) | x <- triangularNumbers]
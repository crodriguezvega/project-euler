import Data.List

problem23 :: Int
problem23 = abs diff
            where diff = total - totalSumOfAbundant
                  total = sum [1..threshold]
                  totalSumOfAbundant = sum $ unique $ sumOfAbundant $ abundant

sumOfAbundant :: [Int] -> [Int]
sumOfAbundant []         = []
sumOfAbundant all@(x:xs) = [x + y | y <- all, x + y <= threshold] ++ sumOfAbundant xs

abundant :: [Int]
abundant = map (\xs -> head xs) $ filter (\xs -> (head xs) < (sum $ tail xs))
                                $ map divisors [(x, [1..isqrt x]) | x <- [1..threshold]]  

-- head of output list is the input number
divisors :: (Int, [Int]) -> [Int]
divisors (n, [])     = []
divisors (n, (x:xs)) = case n `mod` x of 0 -> if x == y then x : divisors (n, xs)
                                              else y : x : divisors (n, xs)
                                                where y = quot n x
                                         _ -> divisors (n, xs)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

threshold :: Int
threshold = 28123

unique :: [Int] -> [Int]
unique = map (\(x:_) -> x) . group . sort
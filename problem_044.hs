import Data.Fixed

problem44 = case findPair pentagonalNumbers pentagonalNumbers of
              Just (x, n) -> x - n
              Nothing     -> 0 

findPair :: [Int] -> [Int] -> Maybe (Int, Int)
findPair _   []     = Nothing
findPair all (x:xs) =
  let list       = takeWhile (<x) all
      candidates = [n | n <- list, isPentagonal (x - n) && isPentagonal (x + n)]
   in if length candidates > 0 then Just (x, head candidates) else findPair all xs

pentagonalNumbers :: [Int]
pentagonalNumbers = [toPentagonal n | n <- [1..3000]]

toPentagonal :: Int -> Int
toPentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Int -> Bool
isPentagonal x = let r = sqrt $ fromIntegral (1 + 24 * x) in r `mod'` 6 == 5
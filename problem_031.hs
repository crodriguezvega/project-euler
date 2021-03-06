import Data.List as List
import Data.Foldable
import Data.Sequence

problem31 :: Int
problem31 = total !! 200
  where total = calculate ways coins

target :: Int
target = 200

ways :: [Int]
ways = [1] ++ List.replicate target 0

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

calculate :: [Int] -> [Int] -> [Int]
calculate ways []     = ways
calculate ways (x:xs) = calculate updatedWays xs
  where updatedWays = calculateWays ways x [x..target]

calculateWays :: [Int] -> Int -> [Int] -> [Int]
calculateWays ways coin []     = ways
calculateWays ways coin (x:xs) = calculateWays updatedWays coin xs
  where updatedWays = toList $ update x updatedValue $ fromList ways
        updatedValue = ways !! x + ways !! (x - coin) 
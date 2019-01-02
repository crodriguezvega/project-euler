import qualified Data.Map as Map

problem53 :: Int
problem53 = length $ filter (>1000000) $ [combinations n r | n <- [1..100], r <- [1..n]]

combinations :: Integer -> Integer -> Integer
combinations n r = factorial n `div` (factorial r * factorial (n - r))

factorial :: Integer -> Integer
factorial n = factorials Map.! n

factorials :: Map.Map Integer Integer
factorials = Map.fromList $ zip [0..100] $ scanl (*) 1 [1..100]
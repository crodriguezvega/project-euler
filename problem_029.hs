import Data.List

problem29 :: Int
problem29 = length $ nub [a^b | a <- [2..100], b <- [2..100]]
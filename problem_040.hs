import Data.Char

problem40 = product $ map (\x -> d digits x) positions
  where digits = take 1000000 $ concat $ map show [1..]

d digits pos = digitToInt $ digits !! (pos - 1)

positions = [1, 10, 100, 1000, 10000, 100000, 1000000]
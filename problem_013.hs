import Data.Char
import Data.Array 

problem13 = do
  content <- readLines "files/problem_013.txt"
  return $ reverse $ sumColumns $ sumRows 50 $ toArray $ toIntegers content

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toIntegers :: [String] -> [[Int]]
toIntegers = map $ map digitToInt

toArray :: [[Int]] -> Array (Int, Int) Int
toArray m = listArray ((1, 1), (100, 50)) $ concat m

sumRows :: Int -> Array (Int, Int) Int -> [Int]
sumRows col arr
  | col > 0   = res : sumRows (col - 1) arr
  | otherwise = []
  where res = foldr (\i acc -> (!) arr (i, col) + acc) 0 [1..100]
  
sumColumns :: [Int] -> [Int]
sumColumns []       = []
sumColumns (x:[])   = x : sumColumns []
sumColumns (x:y:xs) = x `mod` 10 : sumColumns (carryOver:xs)
  where carryOver = y + quot x 10
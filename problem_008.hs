import Data.Char

toIntegers :: String -> [Int]
toIntegers [] = []
toIntegers (x:xs) = digitToInt x : toIntegers xs

windowProduct :: (Num a) => [a] -> [a]
windowProduct xs
  | length xs >= 13 = ( foldr (*) 1 $ take 13 xs) : windowProduct (tail xs)
  | otherwise       = []

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

problem8 = do
  content <- readLines "files/problem_008.txt"
  return (maximum $ windowProduct $ toIntegers $ concat content)
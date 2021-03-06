import Data.Char

problem8 = do
  content <- readLines "files/problem_008.txt"
  return $ maximum $ windowProduct $ toIntegers $ concat content
  
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toIntegers :: String -> [Int]
toIntegers = map digitToInt

windowProduct :: (Num a) => [a] -> [a]
windowProduct xs
  | length xs >= 13 = (foldr (*) 1 $ take 13 xs) : windowProduct (tail xs)
  | otherwise       = []
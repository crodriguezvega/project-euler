import Data.Char

toIntegers :: String -> [Int]
toIntegers [] = []
toIntegers (x:xs) = digitToInt x : toIntegers xs

windowProduct :: (Num a) => [a] -> [a]
windowProduct all@(x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:xs) =
  let product = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12*x13 in product : windowProduct (tail all)
windowProduct _ = [] 

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

problem8 = do
  content <- readLines "files/problem_008.txt"
  return (maximum $ windowProduct $ toIntegers $ concat content)
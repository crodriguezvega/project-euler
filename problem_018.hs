problem18 = do
  content <- readLines "files/problem_018.txt"
  return $ head $ maxSumPath $ reverse $ map readNumbers content

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: String -> [Int]
readNumbers = map read . words

maxSumPath :: [[Int]] -> [Int]
maxSumPath (x:[])     = x
maxSumPath (x0:x1:xs) = maxSumPath $ (maxSum x1 x0):xs

maxSum :: [Int] -> [Int] -> [Int]
maxSum [] _              = []
maxSum (x:xs) (y0:y1:ys) = x + max : maxSum xs (y1:ys)
                           where max = if y0 >= y1 then y0 else y1
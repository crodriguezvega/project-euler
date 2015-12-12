import Data.Array

products :: Int -> Int -> Array (Int, Int) Int -> [Int]
products x y arr
  | x < 18 && y < 4               = vProduct : rdProduct : hProduct : products x (y + 1) arr
  | x < 18 && y >= 4 && y < 18    = vProduct : rdProduct : ldProduct : hProduct : products x (y + 1) arr
  | x < 18 && y >= 18 && y < 20   = vProduct : ldProduct : products x (y + 1) arr
  | x < 18 && y == 20             = vProduct : ldProduct : products (x + 1) 1 arr
  | x >= 18 && x <= 20 && y < 18  = hProduct : products x (y + 1) arr
  | x >= 18 && x <= 20 && y >= 18 = products (x + 1) 1 arr
  | otherwise                     = []
  where vProduct  = (!) arr (x, y) * (!) arr (x + 1, y) * (!) arr (x + 2, y) * (!) arr (x + 3, y)
        rdProduct = (!) arr (x, y) * (!) arr (x + 1, y + 1) * (!) arr (x + 2, y + 2) * (!) arr (x + 3, y + 3)
        ldProduct = (!) arr (x, y) * (!) arr (x + 1, y - 1) * (!) arr (x + 2, y - 2) * (!) arr (x + 3, y - 3)
        hProduct  = (!) arr (x, y) * (!) arr (x, y + 1) * (!) arr (x, y + 2) * (!) arr (x, y + 3) 

toArray :: [[Int]] ->  Array (Int, Int) Int
toArray m = listArray ((1, 1), (20, 20)) $ concat m

readNumbers :: String -> [Int]
readNumbers = map read . words

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

problem11 = do
  content <- readLines "files/problem_011.txt"
  return $ maximum $ products 1 1 $ toArray $ map readNumbers content
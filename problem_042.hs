import Data.List

problem42 :: IO Int
problem42 = do
  content <- readFile "files/problem_042.txt"
  return $ length
         $ filter isTriangleWord
         $ map calculateValue 
         $ readWords content

isTriangleWord :: Int -> Bool
isTriangleWord value = elem value $ takeWhile (<= value) triangleNumbers

calculateValue :: [Char] -> Int
calculateValue = sum . map (\x -> position x alphabet)

position :: Eq a => a -> [a] -> Int
position char alphabet =
  case elemIndex char alphabet of
  Just index -> index + 1 
  Nothing -> 0

triangleNumbers :: [Int]
triangleNumbers = [n * (n + 1) `quot` 2 | n <- [1..]]

alphabet :: [Char]
alphabet = ['A'..'Z']

readWords :: [Char] -> [String]
readWords str = map (\x -> strip "\"" x) $ split ',' str

-- http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
strip :: String -> String -> String
strip = filter . flip notElem
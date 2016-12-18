import Data.List

problem42 = do
  content <- readFile "files/problem_042.txt"
  return $ length
         $ filter isTriangleWord
         $ map calculateValue 
         $ readWords content

isTriangleWord value = elem value $ takeWhile (<= value) triangleNumbers
calculateValue = sum . map (\x -> position x alphabet)
position char alphabet =
  case elemIndex char alphabet of
  Just index -> index + 1 
  Nothing -> 0

triangleNumbers = [n * (n + 1) `quot` 2 | n <- [1..]]
alphabet = ['A'..'Z']

readWords str = map (\x -> strip "\"" x) $ split ',' str

-- http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
strip :: String -> String -> String
strip = filter . flip notElem
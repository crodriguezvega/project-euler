import Data.List

problem22 = do
  content <- readFile "files/problem_022.txt"
  return $ totalScore 1 $ sort $ readNames content

readNames :: String -> [String]
readNames str = map (\x -> strip "\"" x) $ split ',' str

-- http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
strip :: String -> String -> String
strip = filter . flip notElem

totalScore :: Int -> [String] -> Int
totalScore _ []     = 0
totalScore i (x:xs) = i * wordValue x + totalScore (i + 1) xs

wordValue :: [Char] -> Int
wordValue []     = 0
wordValue (x:xs) = letterValue x + wordValue xs 

letterValue :: Char -> Int
letterValue 'A' = 1
letterValue 'B' = 2
letterValue 'C' = 3
letterValue 'D' = 4
letterValue 'E' = 5
letterValue 'F' = 6
letterValue 'G' = 7
letterValue 'H' = 8
letterValue 'I' = 9
letterValue 'J' = 10
letterValue 'K' = 11
letterValue 'L' = 12
letterValue 'M' = 13
letterValue 'N' = 14
letterValue 'O' = 15
letterValue 'P' = 16
letterValue 'Q' = 17
letterValue 'R' = 18
letterValue 'S' = 19
letterValue 'T' = 20
letterValue 'U' = 21
letterValue 'V' = 22
letterValue 'W' = 23
letterValue 'X' = 24
letterValue 'Y' = 25
letterValue 'Z' = 26
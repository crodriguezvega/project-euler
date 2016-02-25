import Data.List

problem24 :: Int
problem24 = fromDigits $ permutation (reverse [0..9]) 0 [0..9]

permutation :: [Int] -> Int -> [Int] -> [Int]
permutation [] _ _     = []
permutation (x:xs) t d = digit : permutation xs total digits
                         where p = progress t x
                               total =  fst p
                               position = snd p
                               digit = d !! position
                               digits = delete digit d
                          
progress :: Int -> Int -> (Int, Int)
progress total num = last $ takeWhile (\x -> fst x < 1000000) [(total + f * x, x) | x <- [0..9]]
                     where f = factorial num

factorial :: Int -> Int
factorial n = product [1..n]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
             where addDigit num d = 10 * num + d
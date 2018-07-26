problem43 :: Integer
problem43 = sum $ map fromDigits
                $ filter isSubStringDivisible
                $ perms [0..9]

isSubStringDivisible :: [Integer] -> Bool
isSubStringDivisible digits = checkDivisibility subStrings [2, 3, 5, 7, 11, 13, 17]
  where subStrings = map subString [1..9]
        subString start = fromDigits $ take 3 $ drop start digits

checkDivisibility :: Integral a => [a] -> [a] -> Bool
checkDivisibility [] _          = True
checkDivisibility _ []          = True
checkDivisibility (x:xs) (y:ys) =
  if x `rem` y == 0 then checkDivisibility xs ys
  else False

fromDigits :: [Integer] -> Integer
fromDigits = foldl (\acc x -> 10 * acc + x) 0

-- http://stackoverflow.com/questions/2710713/algorithm-to-generate-all-possible-permutations-of-a-list
perms :: [a] -> [[a]]
perms (a:as) = [bs ++ a:cs | perm <- perms as, (bs, cs) <- splits perm]
perms []     = [[]]

splits :: [a] -> [([a], [a])]
splits []     = [([], [])]
splits (a:as) = ([], a:as) : [(a:bs, cs) | (bs, cs) <- splits as]
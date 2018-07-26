{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell

  All pandigital numbers except 4 and 7 digit ones are divisible by 3 and thus can’t be primes:
  http://www.mathblog.dk/project-euler-41-pandigital-prime/
-} 

import Data.Ord
import Data.List
import Data.List (inits)
import Data.Array.Unboxed

problem41 :: Int
problem41 = head $ filter isPrime pandigitals 
  where primes = takeWhile (<= 7654321) primesSAE
        isPrime n = elem n primes
        pandigitals = reverse $ sort $ pandigital 7 ++ pandigital 4

pandigital7Digits :: [Int]
pandigital7Digits = pandigital 7

pandigital4Digits :: [Int]
pandigital4Digits = pandigital 4

pandigital :: Int -> [Int]
pandigital n = map fromDigits $ perms [1..n]

fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> 10 * acc + x) 0

-- http://stackoverflow.com/questions/2710713/algorithm-to-generate-all-possible-permutations-of-a-list
perms :: [a] -> [[a]]
perms (a:as) = [bs ++ a:cs | perm <- perms as, (bs, cs) <- splits perm]
perms []     = [[]]

splits :: [a] -> [([a], [a])]
splits []     = [([], [])]
splits (a:as) = ([], a:as) : [(a:bs, cs) | (bs, cs) <- splits as]

primesSAE :: [Int]
primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
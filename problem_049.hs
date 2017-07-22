{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
-}

import Data.List
import Data.Array.Unboxed

problem49 = [x | x <- permutations, length x > 1]
  where permutations = concat $ map (group . sort) $ map (concat . calculateDifferences) primeGroups
        primeGroups = [x | x <- group $ sort $ map toPrime primes, length x > 2]

calculateDifferences :: [Prime] -> [[Difference]]
calculateDifferences [] = [[]] 
calculateDifferences (x:xs) = (map (\y -> Difference (number x) (number y) ((number y - number x))) xs) : calculateDifferences xs

toPrime prime = Prime (digits prime) prime
  where digits = fromDigits . sort . reverse . toDigits

fromDigits = foldl (\acc x -> 10 * acc + x) 0

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

primes = takeWhile (<10000) $ dropWhile (<999) primesSAE

primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft

data Difference = Difference {prime1 :: Int, prime2 :: Int, difference :: Int}

instance Eq Difference where
  (Difference _ p2 diff1) == (Difference p1 _ diff2) = (diff1 == diff2) && (p1 == p2)

instance Ord Difference where
  (Difference _ _ diff1) `compare` (Difference _ _ diff2) = diff1 `compare` diff2

instance Show Difference where
  show (Difference p1 p2 diff) = show p1 ++ " " ++ show p2 ++ " " ++ show diff

data Prime = Prime {digits :: Int, number :: Int}

instance Eq Prime where
  (Prime d1 _) == (Prime d2 _) = (d1 == d2)

instance Ord Prime where
  (Prime d1 _) `compare` (Prime d2 _) = d1 `compare` d2

instance Show Prime where
  show (Prime d n) = show d ++ " " ++ show n
{-
  Using the prime number generator implementation from:
  https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Prime_number_generation#Haskell
-}

import Data.Ord
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = putStrLn $ show problem50

problem50 :: Int
problem50 = snd $ maximumBy (comparing fst)
                $ filter (\x -> isPrime $ snd x)
                $ concatMap calculate
                $ reverse [1..count]
            where count = length primes

million :: Int
million = 1000000

isPrime :: Int -> Bool
isPrime n = elem n primes

cumulativeSums :: [Int]
cumulativeSums = scanl1 (+) primes

primes :: [Int]
primes = takeWhile (<million) primesSAE

calculate :: Int -> [(Int, Int)]
calculate count = dropWhile (\x -> snd x > million) $ calculate' (0:sums) (last sums)
  where sums = take count cumulativeSums

calculate' :: Num t => [t] -> t -> [(Int, t)]
calculate' [] _           = []
calculate' (x:[]) _       = []
calculate' (x:xs) lastSum = (length xs, lastSum - x) : calculate' xs lastSum

primesSAE :: [Int]
primesSAE = 2 : sieve 3 4 (tail primesSAE) (inits primesSAE)
  where
  sieve x q ps (fs:ft) = [i | (i,True) <- assocs (
         accumArray (\ _ _ -> False)
                    True  (x,q-1)
                    [(i,()) | p <- fs, let c = p * div (x+p-1) p,
                              i <- [c, c+p..q-1]] :: UArray Int Bool )]
         ++ sieve q (head ps^2) (tail ps) ft
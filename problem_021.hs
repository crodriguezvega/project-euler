{--
  Amicable numbers:
  https://en.wikipedia.org/wiki/Amicable_numbers
--}

import qualified Data.Map as Map

type IntMap = Map.Map Int Int

problem21 :: Int
problem21 = sumAmicable $ amicable Map.empty
                        $ map divisors [(x, [1..isqrt x]) | x <- [1..9999]]

sumAmicable :: [Maybe (Int, Int)] -> Int
sumAmicable []     = 0
sumAmicable (x:xs) = case x of Nothing              -> sumAmicable xs
                               Just (number, total) -> number + total + sumAmicable xs

amicable :: IntMap -> [[Int]] -> [Maybe (Int, Int)]
amicable m []     = []
amicable m (x:xs) = pair : amicable m' xs
                    where number         = head x
                          properDivisors = tail x
                          total          = sum properDivisors
                          pair           = if (isAmicable m number total) == True
                                            then Just (number, total)
                                            else Nothing
                          m'             = Map.insert number total m

isAmicable :: IntMap -> Int -> Int -> Bool
isAmicable m number total = case (Map.lookup total m) of
                              Nothing -> False
                              Just _  -> (m Map.! total == number)

divisors :: (Int, [Int]) -> [Int]
divisors (n, [])     = []
divisors (n, (x:xs)) = case n `mod` x of 0 -> y : x : divisors (n, xs)
                                              where y = quot n x
                                         _ -> divisors (n, xs)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
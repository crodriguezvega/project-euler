import Data.List
import Data.Ord

problem14 :: Integer
problem14 = fst $ maximumBy (comparing snd) $ map (\n -> (n, collatzLength n)) [1..1000000]

collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength number = 1 + collatzLength x
  where x = if even number then quot number 2 else 3 * number + 1
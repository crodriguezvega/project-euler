import Data.List
import Data.Ord

memoizedCollatzLength :: Int -> Int
memoizedCollatzLength = (map collatzLength [0..] !!)
  where collatzLength 1 = 1
        collatzLength number = 1 + memoizedCollatzLength x
          where x = if even number then quot number 2 else 3 * number + 1

problem14 :: Int
problem14 = fst $ maximumBy (comparing snd) $ map (\n -> (n, memoizedCollatzLength n)) [1..10000]
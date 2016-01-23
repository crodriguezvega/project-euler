problem2 :: Int
problem2 =
  let evenFibonacci = [fib | n <- [1..], let fib = memoizedFib n, fib `mod` 2 == 0]
  in sum $ takeWhile (<= 4000000) evenFibonacci

memoizedFib :: Int -> Int
memoizedFib = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = memoizedFib (n - 2) + memoizedFib (n - 1)
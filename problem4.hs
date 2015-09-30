problem4 :: Int
problem4 =
  let palindromes = [x * y | x <- [100..999], y <- [100..999], isPalindrome $ x * y]
  in maximum palindromes
  where isPalindrome n = show n == (reverse $ show n)
problem36 :: Int
problem36 = sum base10and2Palindromes
  where base10and2Palindromes = filter (\x -> isBinaryPalindrome x) base10Palindromes
        base10Palindromes = filter (\x -> isDecimalPalindrome x) [1..1000000]

isPalindrome digits = digits == reverse digits
isDecimalPalindrome = isPalindrome . toDigits
isBinaryPalindrome = isPalindrome . toBinary

-- http://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
toBinary n = reverse (helper n)
  where helper 0 = []
        helper n = let (q, r) = n `divMod` 2 in r : helper q

toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)
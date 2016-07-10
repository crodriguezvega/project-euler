import Data.List

problem32 :: Int
problem32 = sum $ nub
                $ map third
                $ filter isPandigital products

oneDigit = [1..9]
twoDigit = [(x, y) | x <- oneDigit, y <- oneDigit \\ [x]]
threeDigit = [(x, y, z) | (x, y) <- twoDigit, z <- oneDigit \\ [x, y]]
fourDigit = [(x, y, z, w) | (x, y, z) <- threeDigit, w <- oneDigit \\ [x, y, z]]

twoDigitToNumber (x, y) = x * 10 + y
threeDigitToNumber (x, y, z) = x * 100 + y * 10 + z
fourDigitToNumber (x, y, z, w) = x * 1000 + y * 100 + z * 10 + w

productOneAndFour = [(x, y, x * y) | x <- oneDigit, y <- fourDigitNumbers]
  where fourDigitNumbers = map fourDigitToNumber fourDigit

productTwoAndThree = [(x, y, x * y) | x <- twoDigitNumbers, y <- threeDigitNumbers]
  where twoDigitNumbers = map twoDigitToNumber twoDigit
        threeDigitNumbers = map threeDigitToNumber threeDigit

products = productOneAndFour ++ productTwoAndThree

isPandigital (x, y, z) = (length digits == 9) && (null $ [1..9] \\ nub digits)
  where digits = toDigits x ++ toDigits y ++ toDigits z

toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

third (_, _, x) = x
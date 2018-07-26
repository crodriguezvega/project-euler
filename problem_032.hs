import Data.List

problem32 :: Int
problem32 = sum $ nub
                $ map third
                $ filter isPandigital products

oneDigit :: [Int]
oneDigit = [1..9]

twoDigit :: [(Int, Int)]
twoDigit = [(x, y) | x <- oneDigit, y <- oneDigit \\ [x]]

threeDigit :: [(Int, Int, Int)]
threeDigit = [(x, y, z) | (x, y) <- twoDigit, z <- oneDigit \\ [x, y]]

fourDigit :: [(Int, Int, Int, Int)]
fourDigit = [(x, y, z, w) | (x, y, z) <- threeDigit, w <- oneDigit \\ [x, y, z]]

twoDigitToNumber :: (Int, Int) -> Int
twoDigitToNumber (x, y) = x * 10 + y

threeDigitToNumber :: (Int, Int, Int) -> Int
threeDigitToNumber (x, y, z) = x * 100 + y * 10 + z

fourDigitToNumber :: (Int, Int, Int, Int) -> Int
fourDigitToNumber (x, y, z, w) = x * 1000 + y * 100 + z * 10 + w

productOneAndFour :: [(Int, Int, Int)]
productOneAndFour = [(x, y, x * y) | x <- oneDigit, y <- fourDigitNumbers]
  where fourDigitNumbers = map fourDigitToNumber fourDigit

productTwoAndThree :: [(Int, Int, Int)]
productTwoAndThree = [(x, y, x * y) | x <- twoDigitNumbers, y <- threeDigitNumbers]
  where twoDigitNumbers = map twoDigitToNumber twoDigit
        threeDigitNumbers = map threeDigitToNumber threeDigit

products :: [(Int, Int, Int)]
products = productOneAndFour ++ productTwoAndThree

isPandigital :: (Int, Int, Int) -> Bool
isPandigital (x, y, z) = (length digits == 9) && (null $ [1..9] \\ nub digits)
  where digits = toDigits x ++ toDigits y ++ toDigits z

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

third :: (a, b, c) -> c
third (_, _, x) = x
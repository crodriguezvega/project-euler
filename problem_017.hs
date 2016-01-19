toWord :: Int -> String
toWord 0  = ""
toWord 1  = "one"
toWord 2  = "two"
toWord 3  = "three"
toWord 4  = "four"
toWord 5  = "five"
toWord 6  = "six"
toWord 7  = "seven"
toWord 8  = "eight"
toWord 9  = "nine"
toWord 10 = "ten"
toWord 11 = "eleven"
toWord 12 = "twelve"
toWord 13 = "thirteen"
toWord 14 = "fourteen"
toWord 15 = "fifteen"
toWord 16 = "sixteen"
toWord 17 = "seventeen"
toWord 18 = "eighteen"
toWord 19 = "nineteen"
toWord n
  | n >= 20 && n < 30  = "twenty"
  | n >= 30 && n < 40  = "thirty"
  | n >= 40 && n < 50  = "forty"
  | n >= 50 && n < 60  = "fifty"
  | n >= 60 && n < 70  = "sixty"
  | n >= 70 && n < 80  = "seventy"
  | n >= 80 && n < 90  = "eighty"
  | n >= 90 && n < 100 = "ninety"
  | otherwise          = error "error"

remainderToWord :: Int -> String
remainderToWord r
  | r < 20    = toWord r
  | otherwise = toWord r ++ toWord (r `mod` 10)
  
quotientToWord :: Int -> Int -> String
quotientToWord q r
  | q == 0          = ""
  | q > 0 && q < 10 = toWord q ++ "hundred" ++ and
  | otherwise       = toWord (q `div` 10) ++ "thousand"
                      where and = if r > 0 then "and" else ""

toWords :: Int -> String
toWords n = quotientToWord quotient remainder ++ remainderToWord remainder
            where quotient = n `div` 100
                  remainder = n `mod` 100

problem17 :: Int
problem17 = sum $ map length $ map toWords [1..1000]
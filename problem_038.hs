import Data.List

problem38 = maximum results
  where results = map perform inputs
        inputs = concat ranges

ranges = [[9000..9876], [900..987], [90..98]]
perform n = cproduct n [1..9] []

cproduct _ [] acc     = 0
cproduct n (x:xs) acc = 
  if isPandigital newAcc
    then fromDigits newAcc
    else cproduct n xs newAcc
  where newAcc = acc ++ digits
        digits = toDigits (x * n)

isPandigital digits = l1 == 9 && l2 == 0
  where l1 = length digits
        l2 = length diff
        diff = [1..9] \\ digits

fromDigits :: Num a => [a] -> a
fromDigits = foldl (\acc x -> 10 * acc + x) 0

toDigits n =
  let digits 0 = []
      digits n = n `mod` 10 : digits (n `div` 10)
  in reverse $ digits n
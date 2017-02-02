problem45 = head $ dropWhile condition triangleNumbers
  where condition n = not (isPentagonal n && isHexagonal n)

triangleNumbers = [tr | n <- [286..], let tr = n * (n + 1) / 2]

isPentagonal n = isInt ((sqrt (24 * n + 1) + 1) / 6)

isHexagonal n = isInt ((sqrt (8 * n + 1) + 1) / 4)

isInt n = n == fromInteger (round n)
problem45 :: Double
problem45 = head $ dropWhile condition triangleNumbers
  where condition n = not (isPentagonal n && isHexagonal n)

triangleNumbers :: [Double]
triangleNumbers = [tr | n <- [286..], let tr = n * (n + 1) / 2]

isPentagonal :: (RealFrac a, Floating a) => a -> Bool
isPentagonal n = isInt ((sqrt (24 * n + 1) + 1) / 6)

isHexagonal :: (RealFrac a, Floating a) => a -> Bool
isHexagonal n = isInt ((sqrt (8 * n + 1) + 1) / 4)

isInt :: RealFrac a => a -> Bool
isInt n = n == fromInteger (round n)
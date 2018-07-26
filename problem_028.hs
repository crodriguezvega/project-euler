problem28 :: Int
problem28 = 1 + spiralDiagonals 1 [2, 4 .. 1000]

spiralDiagonals :: Int -> [Int] -> Int
spiralDiagonals n []             = 0
spiralDiagonals n (increment:xs) = total + spiralDiagonals next xs
  where next = head corners
        total = sum corners
        corners = map (\x -> x * increment + n) [4, 3 .. 1]
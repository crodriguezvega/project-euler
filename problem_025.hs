{--
  https://facwiki.cs.byu.edu/ACMCompProg/index.php/Compute_the_Length_of_the_nth_Fibonacci_Number
  http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html
--}

problem25 :: Integer
problem25 = snd $ head
                $ dropWhile (\x -> (fst x) < 1000) [(fibLength x, x) | x <- [1..]]

fibLength :: Integer -> Integer
fibLength n = floor (fromIntegral n * constantOne - constantTwo) + 1

constantOne :: Double
constantOne = logBase 10 goldenRatio

constantTwo :: Double
constantTwo = logBase 10 (sqrt 5)

goldenRatio :: Double
goldenRatio = (1 + sqrt 5) / 2
{--
  We do not need to count the Sundays on the first day of the month for year 1900,
  but we know that January 1st, 1901 was Tuesday, so we start counting from that day.
--}

problem19 :: Int
problem19 = sundaysYear [1901..2000] Tuesday

data Month = January | February | March | April | May | June | July | August | September | October | November | December
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
           
sundaysYear :: [Int] -> Day -> Int
sundaysYear [] _           = 0
sundaysYear (year:ys) day  = nrSundays + sundaysYear ys firstDayNextYear
                             where isLeapYear       = if year `mod` 100 == 0 then year `mod` 400 == 0 else year `mod` 4 == 0 
                                   firstDays        = sundaysMonth [(minBound::Month)..] day isLeapYear
                                   nrSundays        = length $ filter (==Sunday) $ take 12 firstDays
                                   firstDayNextYear = last firstDays

sundaysMonth :: [Month] -> Day -> Bool -> [Day]
sundaysMonth [] day _                  = [day] -- This is the first day of next year
sundaysMonth (month:ms) day isLeapYear = day : sundaysMonth ms firstDayNextMonth isLeapYear
                                         where nrOfDays          = days month isLeapYear
                                               increment         = nrOfDays `mod` 7
                                               firstDayNextMonth = addDays day (nrOfDays `mod` 7)

addDays :: Day -> Int -> Day
addDays day 0     = day
addDays day inc
  | day == Sunday = addDays Monday (inc - 1)
  | otherwise     = addDays (succ day) (inc - 1)

days :: Month -> Bool -> Int
days January _   = 31
days February lp = if lp then 29 else 28 
days March _     = 31
days April _     = 30
days May _       = 31
days June _      = 30
days July _      = 31
days August _    = 31
days September _ = 30
days October _   = 31
days November _  = 30
days December _  = 31
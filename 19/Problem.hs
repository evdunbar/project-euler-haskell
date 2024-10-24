import qualified Data.Map.Strict as Map
{-
 - How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 -
 - for each day in the date range, is it the first of the month and is it a sunday
 -}

divBy p q = p `mod` q == 0

isLeapYear n = n `divBy` 4 && (n `divBy` 400 || not (n `divBy` 100))

monthToDays month year
    | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | month `elem` [4, 6, 9, 11] = 30
    | month == 2 = if isLeapYear year then 29 else 28

main = do
    print $ isLeapYear 1600
    print $ isLeapYear 1700
    print $ isLeapYear 2024

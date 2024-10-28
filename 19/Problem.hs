import Data.List
import Data.Map.Strict qualified as Map

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

solution :: Int -> Int -> Int
solution start end = sum count
  where
    years = [start .. end]
    yearMonth = concatMap (\year -> [(year, month) | month <- [1 .. 12]]) years
    isoDates = concatMap (\(year, month) -> [(year, month, day) | day <- [1 .. monthToDays month year]]) yearMonth
    isoDateDayOfWeek = zip isoDates (cycle ["Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday"])
    count = map (\((_, _, date), dayOfWeek) -> fromEnum $ date == 1 && dayOfWeek == "Sunday") isoDateDayOfWeek

main = do
    print $ solution 1901 2000

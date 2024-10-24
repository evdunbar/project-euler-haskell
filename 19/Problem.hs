{-
 - How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 -
 - for each day in the date range, is it the first of the month and is it a sunday
 -}

isDivBy p q = p `mod` q == 0

isLeapYear n = n `isDivBy` 4 && not (n `isDivBy` 400)

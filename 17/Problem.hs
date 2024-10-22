import Data.Char

numToWords :: Int -> String
numToWords n
    | length digitChars == 4 = special !! head digitNums ++ "thousand" ++ numToWords (read (tail digitChars))
    | length digitChars == 3 && n `mod` 100 /= 0 = special !! head digitNums ++ "hundredand" ++ numToWords (read (tail digitChars))
    | length digitChars == 3 = special !! head digitNums ++ "hundred" ++ numToWords (read (tail digitChars))
    | length digitChars == 2 && n > 19 = tens !! (head digitNums - 1) ++ numToWords (read (tail digitChars))
    | otherwise = special !! n
  where
    special = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    digitChars = show n
    digitNums = map digitToInt digitChars

solution n = sum (map (length . numToWords) [1 .. n])

main = do
    print $ solution 5
    print $ solution 1000

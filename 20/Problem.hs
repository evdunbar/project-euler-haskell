import Data.Char

factorial n
    | n == 0 = 1
    | otherwise = product [2 .. n]

solution n = sum $ map digitToInt digits
  where
    digits = show $ factorial n

main = do
    print $ solution 10
    print $ solution 100

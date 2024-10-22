import Data.Char

solution n = sum (map digitToInt (show (2 ^ n)))

main = do
    print $ solution 15
    print $ solution 1000

{-
 - how many combinations of 20 "R"s and 20 "D"s are there?
 - fix 20 "R"s and place the Ds makes 40 choose 20
 -}

factorial n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = product [2 .. n]

choose n k = factorial n `div` (factorial k * factorial (n - k))

solution n = n `choose` div n 2

main = do
    print $ solution 4
    print $ solution 40

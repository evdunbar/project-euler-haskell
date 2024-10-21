isPythagoreanTriple :: (Int, Int, Int) -> Bool
isPythagoreanTriple (a, b, c) = a ^ 2 + b ^ 2 == c ^ 2

tupleProduct (a, b, c) = a * b * c

solution x = tupleProduct matchingTriple
  where
    n = x
    pythagoreanTriples =
        [ (a, b, c)
        | a <- [1 .. x]
        , b <- [2 .. x]
        , a < b
        , c <- [3 .. x]
        , b < c
        , isPythagoreanTriple (a, b, c)
        ]
    matchingTriple = head (filter (\(a, b, c) -> a + b + c == x) pythagoreanTriples)

main = do
    print (solution 1000)

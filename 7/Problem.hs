smallestFactor :: Int -> Int
smallestFactor x = head (filter (\y -> mod x y == 0) [2 .. x]) -- we can use head because list never empty

primeFactorization :: [Int] -> [Int]
primeFactorization xs
    | divisor == dividend = xs
    | otherwise = primeFactorization (divisor : init xs ++ [div dividend divisor])
  where
    dividend = last xs
    divisor = smallestFactor dividend

isPrime :: Int -> Bool
isPrime x = primeFactorization [x] == [x]

solution :: Int -> Int
solution x = (1 : 2 : filter isPrime (filter odd [3 ..])) !! x

main = do
    print (solution 10001)

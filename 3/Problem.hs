smallestFactor :: Int -> Int
smallestFactor x = head (filter (\y -> x `mod` y == 0) [2 .. x])

primeFactorization :: [Int] -> [Int]
primeFactorization xs
    | divisor == dividend = xs
    | otherwise = divisor : primeFactorization (init xs ++ [div dividend divisor])
  where
    dividend = last xs
    divisor = smallestFactor dividend

solution = last . primeFactorization

main = do
    print (solution [600851475143])

import Data.List

smallestFactor :: Int -> Int
smallestFactor x = head (filter (\y -> mod x y == 0) [2 .. x]) -- we can use head because list never empty

primeFactorization :: [Int] -> [Int]
primeFactorization xs
    | divisor == dividend = xs
    | otherwise = primeFactorization (divisor : init xs ++ [div dividend divisor])
  where
    dividend = last xs
    divisor = smallestFactor dividend

maxOccurences x ys = maximum(map (length . filter (==x)) ys)

solution :: Int -> Int
solution x = product (filter (> 1) (zipWith (^) xs factorCount))
  where
    xs = [2..x]
    primeFactors = map (primeFactorization . singleton) xs
    factorCount = map (\y -> maxOccurences y primeFactors) xs

main = do
    print (solution 20)

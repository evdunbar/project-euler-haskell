import Data.List

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral

smallestFactor :: Int -> Int
smallestFactor x = maybe x fst (uncons (filter (\y -> mod x y == 0) [2 .. isqrt x]))

isPrime :: Int -> Bool
isPrime x
    | nextFactor /= x = False
    | otherwise = True
  where
    nextFactor = smallestFactor x

primes = 2 : filter isPrime [3, 5 ..]

solution :: Int -> Int
solution x = sum $ takeWhile (< x) primes

main = do
    print (solution 2000000)

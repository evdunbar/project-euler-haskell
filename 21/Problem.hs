import Data.List

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- numbers < n which divide evenly into n
properDivisors :: Int -> [Int]
properDivisors n = nub divisors
  where
    divisors = 1 : concatMap (\m -> let (quotient, modulus) = divMod n m in if modulus == 0 then [m, quotient] else []) [2 .. isqrt n]

isAmicable :: [Int] -> Int -> Bool
isAmicable dns a = a == db && a /= b
  where
    b = dns !! (a - 1)
    db = dns !! (b - 1)

amicableNumbers :: Int -> [Int]
amicableNumbers n = takeWhile (< n) $ filter (isAmicable dns) ns
  where
    ns = [1 .. n]
    ms = [1 .. ]
    dns = map (sum . properDivisors) ms

solution n = sum $ amicableNumbers n

main = do
    print $ amicableNumbers 300
    print $ solution 10000

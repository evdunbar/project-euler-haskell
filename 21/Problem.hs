isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

properDivisors :: Int -> [Int]
properDivisors n = 1 : concatMap (\m -> let (quotient, modulus) = divMod n m in if modulus == 0 then [m, quotient] else []) [2 .. isqrt n]

amicableNumbers :: ([Int], [Int]) -> ([Int], [Int])
amicableNumbers (xs, y : ys) = (xs, ys)
    where 
amicableNumbers _ = ([], [])

solution n = n
  where
    nums = [1 .. n]

main = do
    print $ properDivisors 1

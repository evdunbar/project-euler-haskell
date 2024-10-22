import Data.List

triangularNumbers :: [Int]
triangularNumbers = build 1 0
  where
    build n acc = let next = acc + n in next : build (n + 1) next

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factorization :: Int -> [Int]
factorization n = concatMap (\m -> let (quotient, modulus) = divMod n m in if modulus == 0 then [m, quotient] else []) [2 .. isqrt n]

moreDivisorsThan n m = (2 + length (factorization m)) > n -- add 2 for factors 1 and m

solution n = head $ filter (moreDivisorsThan n) triangularNumbers

main = do
    print $ solution 5
    print $ solution 500

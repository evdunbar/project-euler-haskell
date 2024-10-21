sumOfSquares x = sum (map (^ 2) [1 .. x])

squareOfSum x = sum [1 .. x] ^ 2

solution x = squareOfSum x - sumOfSquares x

main = do
    print (solution 100)

fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

solution = sum(filter even (takeWhile (<= 4000000) fibonacci))

main = print solution

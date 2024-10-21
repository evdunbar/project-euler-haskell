solution below = sum (filter (\x -> mod x 3 == 0 || mod x 5 == 0) [3..below-1])

main = do
    print (solution 10)
    print (solution 1000)

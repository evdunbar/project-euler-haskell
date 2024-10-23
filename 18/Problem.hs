import Data.Tree

smallTextTriangle =
    "3\n\
    \7 4\n\
    \2 4 6\n\
    \8 5 9 3"

bigTextTriangle =
    "75\n\
    \95 64\n\
    \17 47 82\n\
    \18 35 87 10\n\
    \20 04 82 47 65\n\
    \19 01 23 75 03 34\n\
    \88 02 77 73 07 63 67\n\
    \99 65 04 28 06 16 70 92\n\
    \41 41 26 56 83 40 80 70 33\n\
    \41 48 72 33 47 32 37 16 94 29\n\
    \53 71 44 65 25 43 91 52 97 51 14\n\
    \70 11 33 28 77 73 17 78 39 68 17 57\n\
    \91 71 52 38 17 14 91 43 58 50 27 29 48\n\
    \63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
    \04 62 98 27 23 09 70 98 73 93 38 53 60 04 2"

parseTriangle :: String -> [[Int]]
parseTriangle = map (map read . words) . lines

triangleToTree :: String -> Tree Int
triangleToTree str = buildTree (parseTriangle str) 0 0

buildTree :: [[Int]] -> Int -> Int -> Tree Int
buildTree rows level pos =
    let currentValue = (rows !! level) !! pos
        numRows = length rows
        nextRowLength = length (rows !! (level + 1))
        hasLeftChild = level + 1 < numRows && pos < nextRowLength
        hasRightChild = level + 1 < numRows && pos + 1 < nextRowLength
        leftChild =
            ([buildTree rows (level + 1) pos | hasLeftChild])
        rightChild =
            ([buildTree rows (level + 1) (pos + 1) | hasRightChild])
     in Node currentValue (leftChild ++ rightChild)

solution :: Tree Int -> Int
solution tree = sum $ snd $ foldTree f tree
  where
    f value [] = (value, [value])
    f value childPaths =
        let (sums, paths) = unzip childPaths
            maxChildSum = maximum sums
            maxPath = snd $ head $ filter ((== maxChildSum) . fst) $ zip sums paths
         in (value + maxChildSum, value : maxPath)

main = do
    print $ solution (triangleToTree smallTextTriangle)
    print $ solution (triangleToTree bigTextTriangle)

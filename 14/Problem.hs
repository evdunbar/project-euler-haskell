import Data.List
import Data.Maybe

nextCollatz :: Int -> Int
nextCollatz n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

collatzSequence :: Int -> [Int]
collatzSequence n = takeWhile (/= 1) (iterate nextCollatz n)

solution n = startNums !! longestIndex
  where
    startNums = [500001, 500003 .. n - 1]
    collatzSequenceLengths = map (length . collatzSequence) startNums
    longestIndex = fromJust (elemIndex (maximum collatzSequenceLengths) collatzSequenceLengths)

main = do
    print $ solution 1000000

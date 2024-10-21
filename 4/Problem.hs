import Data.List

isPalindrome x = reverse xString == xString
  where
    xString = show x

solution = maximum (filter isPalindrome (map (uncurry (*)) pairs))
  where
    pairs = [(first, second) | first <- reverse [100 .. 999], second <- reverse [100 .. 999]]

main = do
    print solution

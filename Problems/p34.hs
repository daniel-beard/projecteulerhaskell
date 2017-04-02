-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Char

-- Only need 0-9
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial 7 = 5040
factorial 8 = 40320
factorial 9 = 362880
factorial _ = 1

factorialSum :: Int -> Int
factorialSum n = sum $ map (\c -> factorial $ digitToInt c) $ show n

calc :: Int
calc = sum $ [x | x <- [3..9999999], factorialSum(x) == x]

main = do 
    print calc

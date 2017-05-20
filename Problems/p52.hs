-- It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

import Data.Char
import Data.List
import Utils.List

sameDigits :: Integer -> Integer -> Bool
sameDigits x y = do
    sort(show(x)) == sort(show(y))
    
permutedMultiple :: Integer -> Bool 
permutedMultiple value = all (\x -> x == True) $ map (sameDigits value) $ zipWith (*) (repeat value) [2,3,4,5,6]

calc :: Int 
calc = length $ takeWhileInclusive (\x -> x == False) $ map (permutedMultiple) [1..]

main :: IO () 
main = print calc
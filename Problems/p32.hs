-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; 
-- for example, the 5-digit number, 15234, is 1 through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Data.List
import Data.Set(fromList, toList)
import Utils.List

-- sublist [1,2,3,4,5,6] 1 3 -> 2,3,4 (inclusive indexes)
sublist :: [a] -> Int -> Int -> [a]
sublist list start end = take ((end-start)+1) $ drop (start) list

pandigitals :: [[Integer]]
pandigitals = permutations [1..9]

pandigitalProduct :: [Integer] -> [Integer]
pandigitalProduct x = do 
    let c1 = sublist x 0 1
        c2 = sublist x 2 4 
        c3 = sublist x 5 8
        d1 = sublist x 0 0
        d2 = sublist x 1 4 
        d3 = sublist x 5 8 
    if nd(c1) * nd(c2) == nd(c3) then c3 
    else if nd(d1) * nd(d2) == nd(d3) then d3 
    else [0] where nd = listToInt

calc :: Integer
calc = sum $ nub $ map (listToInt) $ map (pandigitalProduct) pandigitals

main :: IO () 
main = print calc
    
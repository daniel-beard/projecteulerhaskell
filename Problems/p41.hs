-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
-- For example, 2143 is a 4-digit pandigital and is also prime.
-- 
-- What is the largest n-digit pandigital prime that exists?

import Data.List
import Data.Char
import Utils.Primes
import Utils.List
   
pandigitals :: Int -> [Int]
pandigitals n = reverse $ sort $ map (listToInt) $ permutations [1..n]

largestPrimeInSet :: [Int] -> [Int]
largestPrimeInSet set = take 1 $ dropWhile (\x -> isPrime' x == False) set
   
calculate :: Int 
calculate = head $ maximum $ map (largestPrimeInSet) $ map (pandigitals) [4..9]
    
main :: IO ()
main = print calculate
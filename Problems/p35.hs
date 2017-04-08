-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- 
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Data.List
import Data.Char
import Utils.Primes

-- The approach I take here, is to generate all the combinations of [1,3,7,9] for lengths 2..6 (then include the single digit primes 2,3,7,9
-- Then check if every rotation of the combinations is prime.
-- This is much faster than checking every rotation of every prime < 1,000,000, but this solution is a little ugly.
-- Runs in about ~7s compared to the above approach that takes ~60s

slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

intListToInt :: [Int] -> Int
intListToInt l = read (map (intToDigit) l) :: Int

allPrimes :: [Int] -- All primes under a million
allPrimes = takeWhile (<10^6) primes

isPrime :: Int -> Bool
isPrime n = elem n allPrimes

-- Generates all digit rotations for a given input, e.g.
-- digitRotations 123 -> [123,231,312]
digitRotations :: Int -> [Int]
digitRotations n = do 
    let digitCount = length $ show n
        digitCycle = cycle $ show n
    [read (slice x (x+digitCount) digitCycle) :: Int | x <- [0..(digitCount-1)]]

-- Checks if all digit rotations are prime
allRotationsPrime :: Int -> Bool
allRotationsPrime n = all (isPrime) $ digitRotations n 
-----------------------------------------------------

-- Instead of checking all the primes, we can generate all combinations of [1,3,9,7] of length 2 to 6
allCombinations :: Int -> [[Int]]
allCombinations size = do 
    let wheel = take (size*4) $ cycle [1,3,7,9]
    filter (\x -> length x == size) $ subsequences wheel 

uniquePrimeCombinations = do 
    [2,3,5,7] ++ (filter (isPrime) $ nub $ map (intListToInt) $ flatten [allCombinations x | x <- [2..6]])

calc = do
    (length $ [x | x <- uniquePrimeCombinations, allRotationsPrime x == True]) 

main :: IO ()
main = do 
    print calc

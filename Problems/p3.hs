-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

import Data.List

value :: Int
value = 600851475143

sqrt' :: Int -> Int
sqrt' n = ceiling $ sqrt(fromIntegral(n)) 

firstFactor :: Int -> [Int]
firstFactor n = take 1 $ filter (\x -> (n `rem` x == 0)) [2..sqrt'(n-1)]

primeFactors :: Int -> [Int]
primeFactors n 
    | firstFactor n == [] = [n]
    | otherwise = (firstFactor n) ++ primeFactors (n `div` (head $ firstFactor n))

calc :: [Int]
calc = primeFactors value 

main :: IO ()
main = do 
    print calc

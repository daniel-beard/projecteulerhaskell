-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
-- 
-- 9 = 7 + 2×1^2
-- 15 = 7 + 2×2^2
-- 21 = 3 + 2×3^2
-- 25 = 7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2
--
-- It turns out that the conjecture was false.
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

import Utils.Primes
import Utils.List

isGoldbachNumber :: Int -> Bool
isGoldbachNumber x = do 
    let p = primesLessThan x
    any (==x) $ [x + 2 * (y^2)| x <- p, y <- [1..54]]
   
calculate :: Int 
calculate = do 
    let oddComposites = filter (\x -> isPrime(x) == False) [1,3..]
    last $ takeWhileInclusive (isGoldbachNumber) oddComposites
    
main :: IO ()
main = print calculate
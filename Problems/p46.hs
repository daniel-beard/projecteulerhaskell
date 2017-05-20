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

isPrime :: Int -> Bool
isPrime i = if i == 1 then True else last (takeWhile (<=i) primes) == i
  
primesLessThan :: Int -> [Int]  
primesLessThan x = reverse $ takeWhile (<x) (1:primes)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

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
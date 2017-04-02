-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

import Utils.Primes

calc :: Int
calc = sum $ takeWhile (\x -> x < 2000000) $ primes

main :: IO ()
main = do
    print calc

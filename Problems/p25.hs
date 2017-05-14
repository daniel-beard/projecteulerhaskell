-- The Fibonacci sequence is defined by the recurrence relation:
-- 
-- Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
-- Hence the first 12 terms will be:
--
-- F1 = 1
-- F2 = 1
-- F3 = 2
-- F4 = 3
-- F5 = 5
-- F6 = 8
-- F7 = 13
-- F8 = 21
-- F9 = 34
-- F10 = 55
-- F11 = 89
-- F12 = 144
-- The 12th term, F12, is the first term to contain three digits.
-- 
-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

-- Given an array with fibonacci terms, this returns and array with the next term at the start.
nextFib :: [Integer] -> [Integer]
nextFib arr = (foldl1 (+) $ take 2 $ arr):arr

fibUntil :: (Integer -> Bool) -> [Integer] -> [Integer]
fibUntil cond list = if cond (head list) then list else fibUntil cond (nextFib list)

calculate :: Int 
calculate = length $ fibUntil (\a -> length(show a) >= 1000) [1,1]

main :: IO ()
main = do 
    print calculate
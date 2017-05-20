-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.List
import Data.Maybe

-- Calculates the length of a collatz sequence from a starting number 
-- E.g. collatzSeq 13 13 0
-- First param is the current elem, second param is the collatz length
collatzSeq :: Int -> Int -> Int
collatzSeq 1 n = n -- terminating state
collatzSeq e n = do
    if odd e then collatzSeq (3*e+1) (n+1)
    else collatzSeq (e `div` 2) (n+1)

calc :: Int 
calc = do
    let collatzNumbers = map applyCollatz [1..999999] where
        applyCollatz n = collatzSeq n 1
    let maxSequence = maximum collatzNumbers
    let maxCollatzElem = fromJust (elemIndex maxSequence collatzNumbers) + 1
    maxCollatzElem

main :: IO ()
main = print calc

module Utils.List
(   takeWhileInclusive,
    listToInt ) where

-- Same as take while, but includes the element that we terminate on. 
-- E.g. takeWhileInclusive (/=3) [1,2,3,4] -> [1,2,3]
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- Converts a list of integrals to a single integral
-- E.g. [1,2,3,4] -> 1234                                         
listToInt :: (Integral a) => [a] -> a
listToInt i = foldl ((+).(*10)) 0 i

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- This was my first attempt, while this works, it's REALLY slow.
-- calc = take 1 $ [x | x <- [1..], all (\y -> x `rem` y == 0) [1..20]]

-- Second, and better: left fold using lcm (smallest possible integer that both x && y divide)

calc :: Int
calc = foldl (lcm) 1 [1..20]

main :: IO ()
main = do 
    print calc
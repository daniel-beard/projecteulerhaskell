module Utils.Primes
( primes ) where

-- Tree merging incremental sieve (with wheel): http://rosettacode.org/wiki/Sieve_of_Eratosthenes#With_Wheel
                   
_Y g = g (_Y g)

_U ((x:xs):t) = x : (union xs . _U  . pairs) t  
    where 
        pairs (xs:ys:t) = union xs ys : pairs t
        union a@(x:xs) b@(y:ys) = case compare x y of
             LT -> x : union  xs b
             EQ -> x : union  xs ys
             GT -> y : union  a  ys

primes :: [Int]   
primes = [2,3,5,7] ++ _Y ( (11:) . gapsW 13 (tail wheel) . _U .
                            map (\p->  
                                map (p*) . dropWhile (< p) $
                                    scanl (+) (p - rem (p-11) 210) wheel) )
 
gapsW k (d:w) s@(c:cs) | k < c     = k : gapsW (k+d) w s    -- set difference
                       | otherwise =     gapsW (k+d) w cs   --   k==c
                         
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:    -- gaps = (`gapsW` cycle [2])
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

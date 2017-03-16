-- Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

calc = head [a*b*c | a <- [1..1000], b <- [1..1000], c <- [1000 - a - b], a^2 + b^2 == c^2]

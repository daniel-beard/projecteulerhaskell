-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

import Data.List

powers :: Integer -> Integer
powers upper = sum $ zipWith (^) [1..upper] [1..upper]

calculate :: String
calculate = reverse $ take 10 $ reverse $ show $ powers 1000

main :: IO ()
main = print calculate
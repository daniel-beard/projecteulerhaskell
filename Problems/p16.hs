-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?

import Data.Char

sumOfDigits :: Integer -> Integer
sumOfDigits n = sum . map (\x -> toInteger $ digitToInt x) $ show n

calc :: Integer
calc = sumOfDigits (2^1000)

main :: IO ()
main = print calc

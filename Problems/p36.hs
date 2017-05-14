-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include leading zeros.)

import Text.Printf

isPalindrome :: String -> Bool
isPalindrome s = do 
    if (head $ reverse s) == '0' then False else s == (reverse s)

calculate :: Int -- Filter by decimal palindromes, then binary palindromes, then sum
calculate = sum $ filter (isPalindrome . printf "%b") $ filter (isPalindrome . show) ([1..999999] :: [Int])

main :: IO ()
main = do 
    print calculate
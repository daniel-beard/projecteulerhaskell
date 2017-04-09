-- A palindromic number reads the same both ways. 
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

palindrome :: (Eq a) => [a] -> Bool
palindrome n = n == (reverse n)

products :: [Integer]
products = [x * y | x <- [100..999], y <- [100..999]]

calc = maximum $ filter (\x -> palindrome $ show x) products

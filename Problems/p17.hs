-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

import Data.Char
import Data.List

singleDigitName :: Int -> String
singleDigitName n = case n of 
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> ""

teen :: Int -> String
teen n = case n of 
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"

doubleDigitName :: Int -> String
doubleDigitName n 
    | n < 10 = singleDigitName n
    | n < 20 = teen n
    | n < 30 = "twenty "   ++ singleDigitName (last $ toIntList n)
    | n < 40 = "thirty "   ++ singleDigitName (last $ toIntList n)
    | n < 50 = "forty "    ++ singleDigitName (last $ toIntList n)
    | n < 60 = "fifty "    ++ singleDigitName (last $ toIntList n)
    | n < 70 = "sixty "    ++ singleDigitName (last $ toIntList n)
    | n < 80 = "seventy "  ++ singleDigitName (last $ toIntList n)
    | n < 90 = "eighty "   ++ singleDigitName (last $ toIntList n)
    | n < 100 = "ninety "  ++ singleDigitName (last $ toIntList n)
    | n == 0 = ""
    where toIntList n = map (digitToInt) $ show n

tripleDigitName :: Int -> String
tripleDigitName n = do
    let firstDigitName = singleDigitName $ digitToInt $ head $ show n
        remainderName  = doubleDigitName (n `mod` 100)
    if length remainderName == 0 then firstDigitName ++ " hundred"
    else firstDigitName ++ " hundred and " ++ remainderName

digitName :: Int -> String
digitName n
    | n == 1000 = "one thousand"
    | n < 100 = doubleDigitName n 
    | n >= 100 = do 
         let firstDigitName = singleDigitName $ digitToInt $ head $ show n
             remainderName  = doubleDigitName (n `mod` 100)
         if length remainderName == 0 then firstDigitName ++ " hundred"
         else firstDigitName ++ " hundred and " ++ remainderName

calc :: Int
calc = sum [length $ filter (/=' ') name | x <- [1..1000], let name = digitName x]

main :: IO ()
main = do 
    print calc
    

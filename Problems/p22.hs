-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import System.IO
import Data.List
import Data.Char

nameScore :: String -> Int
nameScore name = sum $ map (\chr -> (ord chr)-64) name

calc :: [String] -> Int 
calc input = do 
    let nameScores = map (\x -> nameScore x) $ sort input
    sum $ zipWith (*) [1..] nameScores

main :: IO ()
main = do
    withFile "Resources/p22.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        print $ calc $ lines contents)

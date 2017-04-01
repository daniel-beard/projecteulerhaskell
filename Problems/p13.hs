
-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
-- (See Resources/p13.txt)

import System.IO
import Data.Char

calc :: String -> String
calc input = take 10 $ show $ sum . map (\elem -> (read elem :: Integer)) $ lines input

main :: IO ()
main = do
    withFile "Resources/p13.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        putStr $ show $ calc contents)

-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
-- How many such routes are there through a 20×20 grid?

-- Note: This is a combinational problem
-- Split the problem into how many 'ups' and 'rights' movements we can make
-- Then the solution is (ups+rights)! / (rights)! / (ups)!
-- Must use Integer, Int has maxBound that's much to small to handle the first factorial

gridSize :: Integer
gridSize = 20 

factorial :: Integer -> Integer
factorial n = product [1..n]

calculate :: Integer
calculate = (factorial (gridSize + gridSize)) `div` (factorial gridSize) `div` (factorial gridSize)

main :: IO ()
main = do 
    print calculate

-- 'perfects' indicates the name of the function
-- the first Int is the function parameter, which is a type Int (meaning it only takes in integer parameters)
-- the [Int] which is the last parameter of the functions is an output, 
-- this mean the function returns a list of Integers
perfects :: Int -> [Int]

-- perfects interates through the 'int' param passed to it, starting from 1 to itself as seen '[1..digit]'
-- for each number it interates through, it check if it is a perfect number,
-- by passing the number to 'isPerfect' function as seen 'isPerfect (fromIntegral presentNum)'
-- if the function 'isPerfect' returns true, that means the number being interated is perfect,
-- and it gets added to the list via 'presentNum |'
-- at the end of the iteration all elements that are perfect would be added to the list, which is returned back as [Int]
perfects digit  = [presentNum | presentNum <- [1..digit], isPerfect (fromIntegral presentNum)]

-- 'isPerfect' indicates the name of the function responsible for checking if a number is a perfect number
-- the first Int is the function parameter, which is a type Int (meaning it only takes in integer parameters)
-- the Bool which is the return type of the function, that tells if a number is perfect or not 
isPerfect :: Integer -> Bool

-- this iterates from 1 to a spefic number (n-1) (with the number being interated denoted as 'i')
-- and for each number in the iteration it performs the steps below,
-- 'n `mod` i' checks if the number assigned to the variable 'i' is a divisor (mod is a haskell inbuilt function),
-- if it is a divsor, it would be added to the list using 'i|'
-- 'sum []' sums all the divisors available for number 'n' (sum is a haskell inbuilt function)
-- 'n == sum []' checks if the sum of the divisors is equals to 'n' and if it is, n is a perfect number        
isPerfect n =  n == sum [i | i <- [1..n-1], n `mod` i == 0]

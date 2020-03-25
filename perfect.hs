
-- 'perfects' indicates the name of the function
-- the first Int is the function parameter, which is a type Int (meaning it only takes in integer parameters)
-- the [Int] which is the last parameter of the functions is an output, 
-- this mean the function returns a list of Integers
perfects :: Int -> [Int]
perfects digit  = [presentNum | presentNum <- [1..digit], isPerfect (fromIntegral presentNum)]

-- 'isPerfect' indicates the name of the function responsible for checking if a number is a perfect number
-- the first Int is the function parameter, which is a type Int (meaning it only takes in integer parameters)
-- the Bool which is the return type of the function, that tells if a number is perfect or not 
isPerfect :: Integer -> Bool

-- For i from 1 to n-1 (that is, [1, 2, 3, 4 ... n-1])
isPerfect n =  n == sum [i | i <- [1..n-1], n `mod` i == 0]

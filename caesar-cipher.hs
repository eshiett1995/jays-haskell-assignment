import Data.Char

-- function caesar is created here which takes in some parameters
-- the first parameter Int specifies that the 'caesar' function takes in an integer
-- the second parameter is a string and the last parameter is the return type which is a string
cipher :: Int -> Char -> Char
cipher n x
    | isDigit x = chr $ ord '0' + (ord x - ord '0' + n'') `mod` 10
    | isLower x = chr $ ord 'a' + (ord x - ord 'a' + n' ) `mod` 26
    | isUpper x = chr $ ord 'A' + (ord x - ord 'A' + n' ) `mod` 26
    | otherwise = x
    where n'  = n `mod` 26 -- for the letters
          n'' = n `mod` 10 -- for the digits

-- function caesar is created here which takes in some parameters
-- the first parameter Int specifies that the 'caesar' function takes in an integer
-- the second parameter is a string and the last parameter is the return type which is a string
caesar :: Int -> String -> String

-- 'caesar n xs = map (cipher n) xs' defines the mode of operation of the caesar function
-- a map is an inbuilt haskell function that takes in a function and a list (a string is technically a list of char),
-- and returns another list constructed by appling a function (the first argument) to all items in a list
-- the function given to the 'map' is the cipher function which is able 
-- to find the 'nth' letter to the right of the intitial item
caesar n xs = map (cipher n) xs
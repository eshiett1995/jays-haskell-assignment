-- the first line declares the function by giving it a name 'expon',
-- the function also allows 2 parameters of 'Int' and also a return type of Int
-- the second line which has 'expon x 0 = 1' is the base case,
-- which ensures the programme terminates via pattern matching
-- the third line recursively calls the expon method over and over until power equals 0
expon :: Int -> Int -> Int
expon x 0 = 1
expon x power = x * (expon x (power-1))

    
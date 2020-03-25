expon :: Int -> Int -> Int
expon x 0 = 1
expon x power = x * (expon x (power-1))
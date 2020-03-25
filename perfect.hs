perfect :: Int -> [Int]
perfect digit  = [presentNum | presentNum <- [1..digit], isPerfect (fromIntegral presentNum)]

isPerfect :: Integer -> Bool   --function declaration 
isPerfect n =  n == sum [i | i <- [1..n-1], n `mod` i == 0]

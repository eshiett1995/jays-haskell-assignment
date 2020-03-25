import Data.Char

cipher :: Int -> Char -> Char
cipher n x
    | isDigit x = chr $ ord '0' + (ord x - ord '0' + n'') `mod` 10
    | isLower x = chr $ ord 'a' + (ord x - ord 'a' + n' ) `mod` 26
    | isUpper x = chr $ ord 'A' + (ord x - ord 'A' + n' ) `mod` 26
    | otherwise = x
    where n'  = n `mod` 26 -- for the letters
          n'' = n `mod` 10 -- for the digits

caesar :: Int -> String -> String
caesar n xs = map (cipher n) xs
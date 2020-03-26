-- the first line shows a declaration of a function 'merge', this function has 3 generic parameter.
-- by generic it means the list can be of any type (Int, Char, String)
-- the second and third line are base cases, which aids to terminate the recursion should incase any
-- list become empty.
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys[] = ys

-- this is the function definition which tries to sort each element when merging
merge first @ (x:xs) second @(y:ys)
   | x <y = x :merge xs (y:ys)
   | otherwise = y : merge ys (x:xs)
-- the first line shows a declaration of a function 'merge', this function has 3 generic parameter.
-- by generic it means the list can be of any type (Int, Char, String)
-- the second and third line are base cases, which aids to terminate the recursion should incase any
-- list become empty.
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys[] = ys

-- this is the function definition which sorts each element when merging
-- it gets the first element of the second list and compares it with the second element of
-- the first list.
-- if the first element of the second list is lesser it is added to the front of the  list via 'x:'
-- and if it is not then the second element of the first list is added to the front of the list via 'y:'
merge (x:xs) (y:ys)
   | x <y = x :merge xs (y:ys)
   | otherwise = y : merge ys (x:xs)
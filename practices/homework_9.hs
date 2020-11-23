runs::Int->[a]->[[a]]
runs _ [] = []
runs a l   
    | a > length l = [l]
    | otherwise = take a l : runs a (drop a l)

join::[a]->[[a]]->[a]
join _ [] = []
join c l 
    | length l == 1 = head l 
    |otherwise = head l ++ c ++ join c (tail l)
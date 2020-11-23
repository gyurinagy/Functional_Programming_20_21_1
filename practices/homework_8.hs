merge::[Int]->[Int]->[Int]
merge [] l = l  
merge l [] = l 
merge (x:xs) (y:ys) 
    | y < x     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

mergeSort::[Int]->[Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort (take (div (length l) 2) l)) (mergeSort (drop (div (length l) 2) l)) 
--mergeSort l = merge (mergeSort (fst (splitAt (div (length l) 2) l))) (mergeSort (snd (splitAt (div (length l) 2) l)) )
--mergeSort (p:xs) = (mergeSort lesser) ++ [p] ++ (mergeSort greater)
    -- where
    --     lesser  = filter (< p) xs
    --     greater = filter (>= p) xs
    

align::Int->String->String
align n str 
    | n > length str = ' ': align (n-1) str
    | otherwise = str

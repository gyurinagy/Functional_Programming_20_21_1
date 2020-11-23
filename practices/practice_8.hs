sum'::[Int]->Int
sum' l 
    | l==[] = 0
    | otherwise = (head l)+(sum' (tail l))
--sum' [] = 0
--sum' (x:xs) = x + sum' xs

last' :: [Int] -> Int
last' [x] = x
last' (_:xs) = last' xs

and'::[Bool]->Bool
and' [] = True
and' (x:xs) = x && and' xs

or'::[Bool]->Bool
or' [] = False
or' (x:xs) = x || or' xs

repeat'::Char->String
repeat' c = c:(repeat' c)

replicate'::Int->Char->String
--replicate' n c = take n (repeat' c)
replicate' 0 _ = []
replicate' n c = c:(replicate' (n-1) c)

format::Int->String->String
format n s
    -- | n > length s = format n (' ':s)
    | n > length s = ' ' : format (n-1) s
    -- | n > length s = (replicate' (n-lngh) ' ')++s
    | otherwise = s
    --where lngh = length s

insert::Int->[Int]->[Int]
insert n [] = [n]
insert n (x:xs)
    | n > x = x : (insert n xs)
    | otherwise = n:x:xs

sort :: [Int]->[Int]
sort [] = []
sort (x:xs) = insert x (sort xs)

breakOn::Char->String->(String,String)
breakOn c (x:xs)
    |c==x = ("",x:xs)
    | otherwise = (x:b1,b2)
    where (b1,b2) = breakOn c xs

splitOn::Char->String->[String]
splitOn _ [] = []
splitOn c (x:xs)
    |c==x = "":rest
    | rest ==[] = [[x]]
    | otherwise = (x:(head rest)) : tail rest
    where rest = splitOn c xs

csv'::[String]->[[String]]
csv' [] = []
csv' (x:xs) = (splitOn ',' x):csv' xs

csv::String->[[String]]
csv s = csv' (lines s)
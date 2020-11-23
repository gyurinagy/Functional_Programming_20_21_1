repeat'::a->[a]
repeat' x = x:repeat' x

take'::Int->[a]->[a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:take (n-1) xs

drop'::Int->[a]->[a]
drop' 0 l = l 
drop' _ [] = []
drop' n (x:xs) 
    | n <= 0 = x:xs
    | otherwise = drop' (n-1) xs

langAndRegion::String->(String,String)
langAndRegion [a,b,'-',c,d] = ([a,b],[c,d])
langAndRegion _ = error "Na ne má"

zip'::[a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ []  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip'::[(a,b)]->([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xs) = (x:a, y:b)
    where (a,b) = unzip xs

empty'::[String]->Int->[Int]
empty' [] _  = []
empty' ("":xs) n = n:empty' xs (n+1)
empty' (_:xs) n = empty' xs (n+1)

empty::String->[Int]
empty s = empty' (lines s) 1

empty2 :: String -> [Int]
empty2 s = map fst (filter (\(_,s) -> s=="") (zip [1..] (lines s)))

splitAt' :: Int -> [a] -> ([a],[a])
--splitAt' n l = (take n l, drop n l)
splitAt' _ [] = ([],[])
splitAt' n (x:xs)
    | n <= 0 = ([],x:xs)
    | otherwise = (x:ra, rb)
    where (ra,rb) = splitAt' (n-1) xs

nub' :: Eq a => [a] -> [a]
nub' [] = []
{-
nub' (x:xs)
| elem x rest = rest
| otherwise = x: rest
where rest = nub' xs
-}
nub' (x:xs) = x:nub' (filter (\e->e /= x) xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
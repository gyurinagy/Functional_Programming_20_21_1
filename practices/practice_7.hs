fact::Int->Int
face 0 = 1
fact 1 = 1
fact n = n * (fact (n-1))

fib::Int->Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

pow::Int->Int->Int
pow _ 0 = 1
pow x n = pow x (n-1)

range::Int->Int->[Int]
range a b
    | a == b = [a]
    | b < a = a:(range (a-1) b)
    | otherwise = a:(range (a+1) b)

--[a] bármilyen típust tartalmazhat
--length'::[a]->Int
--length' [] = 0
--length' (_:xs)
--    | null xs = 1 
--    | otherwise = 1 + length' xs

length'::[a]->Int
length' l
    | null l = 0
    | otherwise = 1 + length' (tail l)

minimum':: [Int]->Int
minimum' [x] = x
minimum' (x:xs)
    |  x < minimum' xs = x
    | otherwise =minimum' xs
       -- where miin = minimum' xs

minimum2:: [Int]->Int
minimum2 x
    |  length x == 1 = head x
    | head x < minimum2 (tail x) = head x
    | otherwise = minimum2 (tail x)


everySecond::[a]->[a]
everySecond [] = []
everySecond [_] = []
everySecond (_:x:xs) = x: everySecond xs

elem' :: Char->String ->Bool
elem' _ [] = False
elem' c (x:xs)
    |c == x = True
    | otherwise = elem' c xs


value::Int->[(Int,String)]->String
value _ [] = error "Hopphopp, hiba"
value x ((kx,vx):xs)
    | x == kx = vx
    | otherwise = value x xs

--value2::Int->String->[(Int,String)]->String
 
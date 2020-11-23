import Data.Char(toUpper)

sum' :: [Int] -> Int
sum' [] = 0
--head and tail (h:t)
--sum' l = (head l) + sum (tail l)
sum' (h:t) = h + sum t

headInt:: [Int] -> Int
--headInt [x] = x ➞ nem kell
headInt (h:_) = h

tailInt :: [Int] -> [Int]
tailInt (_:t) = t

nullInt::[Int]->Bool
nullInt [] = True
nullInt _ = False

isSingletonInt::[Int]->Bool
isSingletonInt [_] = True
isSingletonInt _ = False

toUpperFirst::String->String
toUpperFirst "" = ""
toUpperFirst (x:xs) = (toUpper x):xs

isLetter:: Char->Bool
--isLetter x = elem x (['a'..'z']++['A'..'Z'])
isLetter c = elem (toUpper c) ['A'..'Z']

isDigit::Char->Bool
isDigit x = elem x['0'..'9']



mountain::Int->[Int]
--mountain 0 = []
mountain x = [1..(x-1)]++[x,(x-1)..1]

divisors::Int->[Int]
divisors 0 = [1..]
divisors x = [i |i<-[1..x], mod x i ==0]

powersOfTwo::[Int]
powersOfTwo = [2^i | i<-[0..]]
-- take 10, megnézi az első x elemét egy tömbnek
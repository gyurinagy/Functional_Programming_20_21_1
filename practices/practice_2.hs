inc:: Int -> Int
inc a = a+1


even' :: Int -> Bool
even' x = x `mod` 2 ==0 

odd' :: Int -> Bool
--odd' x = x `mod` 2 == 1
odd' x = not (even' x)

divides :: Int -> Int -> Bool
divides x y = y `mod` x ==0

area :: Int -> Int -> Int
area x y = x*y

triangleSides :: Int ->Int ->Int ->Bool
triangleSides a b c = (a+b>c) && (a+c>b) && (c+b>a) 


pythagoreanTriple :: Int ->Int ->Int ->Bool
pythagoreanTriple a b c = (a ^2+b^2==c^2) || (a^2+c^2==b^2) || (c^2+b^2==a^2)

isLeapYear :: Int->Bool
isLeapYear y = (divides 4 y ) && (not ( divides 100 y) || (divides 400 y))
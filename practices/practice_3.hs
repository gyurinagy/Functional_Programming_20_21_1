-- Mintha illesztés
viccesinc::Int->Int
viccesinc 3 = 5
viccesinc x = x + 1

hossz :: Double ->Double->Double
hossz 10 _ = 0
hossz _ 5 = 123.0
hossz x y = sqrt(x^2+y^2)

add:: (Int,Int)->(Int,Int)->(Int,Int)
add (a,b) (c,d) = (b*c+a*d,b*d)

mul:: (Int,Int)->(Int,Int)->(Int,Int)
mul (a,b) (c,d) = (a*c,b*d)

modDiv:: Int->Int->(Int,Int)
modDiv x y = (mod x y, div x y )

dis' ::  Double -> Double -> Double -> Double
dis' a b c = sqrt (b^2-4*a*c)

quadratic :: Double -> Double -> Double -> (Double,Double)
--quadratic a b c = ((-b-(sqrt (b^2-4*a*c)))/(2*a),(-b+(sqrt (b^2-4*a*c)))/(2*a))
--quadratic a b c = ((-b-(dis' a b c))/(2*a),(-b+(dis' a b c))/(2*a))
quadratic a b c = ((-b-d)/(2*a),(-b+d)/(2*a))
    where d = sqrt (b^2-4*a*c)


matches :: (Int, Int) ->(Int, Int)->Bool
matches (a,b) (c,d) = a==c || a==d || b==c || b==d

len:: (Int,Int)->Double
len (x,y) = sqrt(fromIntegral (x^2+y^2))

stretch::(Int,Int)->Int->(Int,Int)
stretch (a,b) c = (a*c,b*c)

distance:: (Int,Int)->(Int,Int)->Double
--distance (x1,y1) (x2,y2) = sqrt(fromIntegral ((x1-x2)^2+(y1-y2)^2))
distance (x1,y1) (x2,y2) = len(x1-x2,y1-y2)

-- 4. előadás példák
isPrime :: Integer -> Bool
isPrime n = divisors n == [1]

divisors:: Integer -> [Integer]
divisors n = [m |m <- [1..n], mod n m == 0]
--[kimeneti kifejezés | m az honnan jön, bejárjuk a listát növekvő sorrendben, megnézzük minden alkalommal, hogy a maradékos osztás 0-e ]
-- lista generátor
{-
[2*x|x<-[1..5]]
[2*x|x<-[1..5], even x, x>3]
[x^3|x<-[1..5],y<-[1..3]]
[x^y|x<-[1..5],y<-[1..3],even x]

!! jellel lehet kikérni egy lista adott elemé
[0..]!!100  --> 100

zip [1..5] [5,4..1]
[(1,5),(2,4),...]

take 30 cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,1,2,3,....]

-}

import Data.List

isPrime::Int->Bool
isPrime 1 = False
isPrime 0 = False
isPrime n = []==[x | x<-[2..n-1], mod n x==0] --ha a vissza kapott tömb üres
--isPrime n = null [x |x<-[2..n-1], mod n x==0]
--isPrime n = 2 == length [x |x<-[1..n], mod n x==0]

primes = [n |n<-[1..],isPrime n]

allPositive::[Int]->Bool
allPositive l = null [x |x<-l, x<=0]

dominoes::[(Int,Int)]
dominoes =[(a,b) | a<-[0..6],b<-[0..a]]

alphabet = zip [1..] ['a'..'z']

everyThird = [c |(b,c)<-zip (cycle [False,False,True]) ['a'..'z'], b] -- vissza adja azokat a c karaktereket ahol b == True 
everyThird1 = [snd rp |rp<-zip (cycle [False,False,True]) ['a'..'z'], fst rp]
everyThird2 = ['c','f'..'z']
everyThird3 = [c |(n,c) <-zip [1..] ['a'..'z'], mod n 3==0]

courses::[(String,[(String,String,String)])]
courses =
    [ ("Programozasi nyelvek II.", [("Horvath", "Istvan", "BDE91E"), ("Fodros", "Aniko", "DDA3KX")])
    , ("Imperativ programozas", [("Nemeth", "Eniko", "ALX1K0"), ("Horvath", "Istvan", "BDE91E")])
    , ("Funkcionalis programozas", [("Kiss", "Elemer", "ABCDE6"), ("Nagy", "Jakab", "CDE560")])
    ]

students = [n | (t,d)<-courses,(_,_,n)<-d, t == "Funkcionalis programozas"]
students2 = [neptun | (targy, diakok)<-courses, (_,_,neptun)<-diakok, targy == "Funkcionalis programozas"]


toUpperFirst::[Int]->Int
toUpperFirst (x:xs) = x
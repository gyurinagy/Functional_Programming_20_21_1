import Data.List ( group )
type Dictionary = [(Char, Integer)]

dictionary :: [Char] -> Dictionary
dictionary x = zip x [1..]

dictionary_az = dictionary ['a'..'z']
dictionary_az_AZ = dictionary (['a'..'z']  ++ ['A'..'Z'])

charToNum :: Dictionary -> Char -> Integer
charToNum dic chr  
    | null dic || not (elem chr   [n | (n,_)<-dic]) = 0
    | otherwise = head [n | (c,n)<-dic, c==chr ]

numToChar::Dictionary->Integer->Char
numToChar dic b  
    | null dic || not (elem b  [n | (_,n)<-dic]) = '*'
    | otherwise = head [n | (n,t)<-dic, t==b]

translate :: Dictionary -> String -> [Integer]
translate dic str = [charToNum dic n | n<-str  ] 

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = odd n && null [d | d <- [3,5..squareRoot n], n `mod` d == 0] where
  squareRoot :: Integer -> Integer
  squareRoot n = floor (sqrt (fromIntegral n))

primeList :: [Integer]
primeList = 2:[ x | x <- [3,5..], isPrime x]

encode :: Dictionary -> String -> Integer
encode dic str 
    | null dic || null str  = 1
    | otherwise = product (zipWith (^) (take (length str ) primeList) (translate dic str))

    
primeFactorization :: Integer -> [Integer]
primeFactorization x 
    | isPrime x = [x]
    | x == 1 = []
    | otherwise = i: primeFactorization (div x i)
        where i = head [n | n<-primeList, mod x n == 0]


decode :: Dictionary -> Integer -> String
decode dic n = [numToChar dic f | f<-[fromIntegral( length i) | i<-group (primeFactorization n) ]]
import Data.List

dropSpaces :: String -> String
--dropSpaces s = dropWhile (\c->c==' ') s
--dropSpaces s = dropWhile (==' ') s
dropSpaces = dropWhile (==' ')


trim :: String -> String
trim s = reverse (dropSpaces (reverse (dropSpaces s)))


monogram :: String -> String
monogram s = trim (concat (map (\(c:_)->c:". ") (words s)))


uniq :: Ord a => [a] -> [a]
uniq l = map head (group (sort l))


repeated :: Ord a => [a] -> [a]
--repeated l = map head(filter (\s ->length s >1) (group (sort l)))
repeated l = map head(filter (\(_:t) ->t /= []) (group (sort l)))


add :: Num a => a -> a -> a
add x y = x+y

toPair :: a -> b -> (a, b)
toPair x y = (x,y)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f x y =map (\(xe,ye) -> f xe ye) (zip x y)
--zipWith' f x y =map (uncurry f) (zip x y)


dotProduct :: [Int] -> [Int] -> Int
dotProduct v1 v2 = sum (zipWith (*) v1 v2)


isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\o -> n `mod` o /=0) [2..n-1]

primes :: [Int]
primes = filter isPrime [0..]


iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x:iterate'' f (f x)


fibonacci :: [Integer]
fibonacci = [0,1] ++ zipWith (+) fibonacci (tail fibonacci)

fib :: [Integer]
fib = map fst (iterate (\(x,y)->(y,x+y)) (0,1))


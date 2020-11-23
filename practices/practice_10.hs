import Data.Char

add8::Int->Int
add8 x = x+8

rem8::Int->Int
rem8 x = x-8

map'::(a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x  : map' f xs

filter'::(a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

upperToLower ::String ->String
-- upperToLower  s = map toLower (filter' (`elem`  ['A'..'Z']) s)
upperToLower  s = map toLower (filter' (\c -> elem c ['A'..'Z']) s)

all'::(a->Bool)->[a]->Bool
all' f l = and (map'  f l)


any'::(a->Bool)->[a]->Bool
any' f l = or(map' f l)

hasLongLines::String->Bool
hasLongLines s = any' (\l -> length(words l) >=3) (lines s)

elem'::Eq a =>a->[a] -> Bool
-- elem' e l = any' (\x -> x==e ) l
-- elem' e l = any' (==e) l
elem' e  = any' (==e) 

hasAny::Eq a => [a]->[a]->Bool
hasAny a b = any' (\x->elem' x a) b
-- hasAny a b = any' (`elem'`  a) b
-- hasAny a  = any' (`elem'`  a)

takeWhile' :: (a->Bool)->[a]->[a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = takeWhile' f xs
    | otherwise = []

dropWhile' :: (a->Bool)->[a]->[a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise =x:xs

dropWord :: String->String
dropWord s = dropWhile'(\x->x /= ' ') s
-- dropWord s = dropWhile'( /= ' ') s
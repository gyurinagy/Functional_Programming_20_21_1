--let suni = (1,"hehe")

fst' :: (a,b) ->a
fst' (x,_) = x

snd' :: (a,b) ->b 
snd' (_,x) = x

middle :: (a,b,c) -> b 
middle (_,x,_) = x

and' :: Bool->Bool->Bool
and' True True = True
and' _ _ = False

or' ::Bool->Bool->Bool
or' False False = False
or' _ _ = True

xor' ::Bool->Bool->Bool
xor' True False= True
xor' False True = True
xor' _ _ = False

add2::Int->Int->(Int,Int)
add2 1 1 = (0,1)
add2 x y= (x+y,0)

paren::Char->Char->Bool
paren '(' ')' = True
paren '{' '}' = True
paren '[' ']' =  True
paren _ _ = False

llist :: [Int] -> Int
llist  [5,4,_,a] = 12+a
llist _ =10

-- Csak akkor érdemes x,y stb változó neveket adni paraméterben ha azzal kezdünk is valamit
foo::(Bool,Int,[Int]) ->Int
foo (_,10,[1,_,3,_,x,y]) = x+y
foo _ = 0

calc :: (Int, Char,Int) -> Int
calc (a, '+', b) = a+b 
calc (a, '-', b) = a-b 
calc (a, '*', b) = a*b 
calc (a, '/', b) = a `div` b 

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False
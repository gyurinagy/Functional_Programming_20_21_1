isIdentifierStart :: Char->Bool
isIdentifierStart x = x `elem` ('_':['a'..'z'])

isIdentifierPart :: Char->Bool
isIdentifierPart x = isIdentifierStart x || elem x (['A'..'Z']++['1'..'9']) 

isReserved :: String ->Bool
isReserved x = elem x ["if","then","else","module","import"]

isValid :: String -> Bool
isValid "" = False
isValid str = (isIdentifierStart (head str)  ) &&
 (all isIdentifierPart (tail str) ) &&
  not (isReserved str)

pii = ( sum (zipWith (/) [-(-1)**i|i<-[1..]] [a | a<-[1,3..2000]]))*4


--piii = (sum [((-(-1)**a)/b) | a<-[1..1000], b<-[1,3..2000] ])*4

time::[(Int,Int)]
time = [(a,b) | a<-[0..23],b<-[0..59]]
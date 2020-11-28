import Data.Char ( toUpper )

abc :: [Char]
abc = ['A'..'Z']

isValidLetter :: Char -> [Char] -> Bool
isValidLetter c str = elem (toUpper c) (map toUpper str)

startState :: [Char] -> [Char] -> ([Char],[Char],[Char])
startState abc str
    | and [isValidLetter c abc | c<-str, c/=' '] = (map toUpper str,[],[])
    | otherwise = error "ajjaj"

guessLetter :: [Char] -> Char -> ([Char],[Char],[Char]) -> ([Char],[Char],[Char])
guessLetter abc c (str1,str2,str3)
    | not (isValidLetter c abc) = error "ajjaj"
    | isValidLetter c str2 || isValidLetter c str3 = (str1,str2,str3)
    | isValidLetter c str1 = (str1,toUpper c:str2,str3)
    | otherwise = (str1,str2,toUpper c:str3)


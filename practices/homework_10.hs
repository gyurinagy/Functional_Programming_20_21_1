abc :: [Char]
abc = ['A'..'Z']

isValidLetter::Char->[Char]->Bool
isValidLetter c l= elem c l

-- startState::[Char]->String->([Char],[Char],[Char])
-- startState 
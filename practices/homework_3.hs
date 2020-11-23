nand::Bool->Bool->Bool
nand True True = False
nand a b = True

onAxis::(Int,Int)->Bool
onAxis (0, _) = True
onAxis (_, 0) = True
onAxis (a, b) = False

punctuation::Char->Bool
punctuation '?' = True
punctuation '!' = True
punctuation '.' = True
punctuation c = False
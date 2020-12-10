data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show,Eq)

type Year = Int


numberOfDays :: Year -> Month -> Int
numberOfDays y m
    | m==Jan || m==Mar || m==May || m==Jul || m==Aug || m==Oct || m==Dec = 31 
    | m==Apr || m==Jun || m==Sep || m==Nov = 30
    | mod y 400 == 0 || (mod y 4 == 0 && mod y 100 /= 0) = 29
    | otherwise = 28

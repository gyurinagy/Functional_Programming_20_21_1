data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving Show

isFirstDayOfWeek :: Day -> Bool
isFirstDayOfWeek Mon = True
isFirstDayOfWeek _ = False


szovegge :: Show a => a -> String
szovegge x = show x


isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False


data Time = T Hour Minute
    deriving (Show, Eq)

type Hour = Int
type Minute = Int

--type Stringecske = [Char]

showTime :: Time -> String
showTime (T h m) = show h ++ "." ++ show m

eqTime :: Time -> Time -> Bool
eqTime (T h1 m1) (T h2 m2) = h1*60+m1 == h2*60 + m2

isEarlier :: Time -> Time -> Bool
isEarlier (T h1 m1) (T h2 m2) = h1*60+m1 < h2*60 + m2

isBetween :: Time -> Time -> Time -> Bool
isBetween t1 t2 t3 = (isEarlier t1 t2 && isEarlier t2 t3) || (isEarlier t3 t2 && isEarlier t2 t1)


time :: Hour -> Minute -> Time
time h m 
    | h `mod` 24 /= h = error ("time: invalid hour: " ++ show h)
    | m `mod` 60 /= m = error ("time: invalid minute: " ++ show m)
    | otherwise = T h m



data USTime = AM Hour Minute | PM Hour Minute
    deriving (Eq, Show)

showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

usTimeToTime :: USTime -> Time
usTimeToTime (AM 12 m) = T 0 m
usTimeToTime (AM h m) = T h m
usTimeToTime (PM 12 m) = T 12 m
usTimeToTime (PM h m) = T (h+12) m


timeToUSTime :: Time -> USTime
timeToUSTime (T 0 m) = AM 12 m
timeToUSTime (T 12 m) = PM 12 m
timeToUSTime (T h m)
    | h < 12 = AM h m
    | otherwise = PM (h-12) m


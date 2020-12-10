--data Maybe a = Just a | Nothing


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

showMaybeInt :: Maybe Int -> String
showMaybeInt Nothing = "Nincs it semmi latnivalo kerem!"
showMaybeInt (Just n) = show n


data Privilege = Unprivileged | Admin
    deriving (Eq, Show)

data Cookie = LoggedOut | LoggedIn UserName Privilege
    deriving (Eq, Show)


type UserName = String
type Password = String
type Record = (UserName, Password, Privilege)
type Database = [Record]

db :: Database
db = [("dumbledore","abracadabra",Unprivileged), ("root", "secret", Admin), ("bela", "korte", Unprivileged)]


register :: UserName -> Password -> Cookie -> Database -> Database
register un pw (LoggedIn _ Admin) db = (un, pw, Unprivileged):db
register _ _ _ db = db

{-
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x
trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x
-}

getUser :: UserName -> Database -> Maybe (Password, Privilege)
getUser un db
    | null res = Nothing
    | otherwise = (\(_, pw, priv) -> Just (pw, priv)) (head res)
    where res = filter (\(dbun,_,_)->dbun == un) db
{-getUser _ [] = Nothing
getUser un ((dbun,dbpw,priv):xs)
    | un == dbun = Just (dbpw,priv)
    | otherwise = getUser un xs-}


login :: UserName -> Password -> Database -> Cookie
{-login un pw db
    | null res = LoggedOut
    | otherwise = LoggedIn un ((\(_,_,priv)->priv) (head res))
    where res = filter (\(dbun,dbpw,_)->dbun == un && dbpw == pw) db-}
login _ _ [] = LoggedOut
login un pw ((dbun, dbpw, priv):xs)
    | un== dbun && pw == dbpw = LoggedIn un priv
    | otherwise = login un pw xs

passwd :: Password -> Cookie -> Database -> Database
passwd _ LoggedOut db = db
passwd _ _ [] = []
passwd pw (LoggedIn un _) ((dbun, dbpw, priv):xs)
    | un == dbun = (un,pw,priv):xs
    | otherwise = (dbun, dbpw, priv):passwd pw (LoggedIn un Unprivileged) xs

delete :: UserName -> Cookie -> Database -> Database
delete un (LoggedIn _ Admin) db = filter (\(dbun,_,_)->dbun/=un) db
delete _ _ db = db

users :: Database -> [UserName]
users db = map (\(dbun,_,_)->dbun) db




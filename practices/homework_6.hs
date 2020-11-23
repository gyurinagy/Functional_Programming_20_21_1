import Data.List(group)

table::[(Char,Char)]
table = [(' ',' ')]++(zip ['a'..'z'] ['d'..'z'])++(zip ['x'..'z'] ['a'..'z'])

shift::[(Char,Char)]->Char->Char
shift x d = snd ( head [(a,b) | (a,b)<-x ,a==d])

encrypt::[(Char,Char)]->String->String
encrypt x str = [shift x n | n<-str]

naturalPairs::[(Int,Int)]
naturalPairs = [(a,b) |x<-[0..], a<-[0..x], b<-[0..x], x-a-b==0]

calendar::[(Int,Int)]
calendar = [(m,d) | m<-[2], d<-[1..28]]++
 [(m, d) | m<-[1,3,5,7,8,10,12], d<-[1..31]] ++ 
 [(m,d) | m<-[4,6,9,11], d<-[1..30]]

compress::String->[(Int,Char)]
compress x =[(length (group (x)!!i), head (group (x)!!i)) | i<-[0..length (group (x)) -1]]

decompress::[(Int,Char)]->String
decompress x = concat [replicate (fst(x!!i)) (snd(x!!i)) | i<-[0..length x -1]]
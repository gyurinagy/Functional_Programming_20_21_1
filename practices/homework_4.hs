shift'::(Int,Int)->Int->(Int,Int)
shift' (x,y) z = ((x+f)-((div (x+f) 24)*24),((y+z) -(f*60)))
    where f = (div (y+z) 60)


isEarlier::(Int,Int)->(Int,Int)->Bool
isEarlier (x,y) (z,w) = x<z || (x==z && y<w)

createEvent::(Int,Int)->Int->String->((Int,Int),(Int,Int),String)
createEvent (x,y) z s = ((x,y),((x+f)-((div (x+f) 24)*24),((y+z) -( f*60))),s)
    where f = (div (y+z) 60)
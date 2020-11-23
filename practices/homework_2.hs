acuteTriangle :: Double-> Double-> Double-> Bool
acuteTriangle a b c =  (a^2 + b^2 > c^2) && (c^2 + b^2 > a^2) && (a^2 + c^2 > b^2) 

grade1:: Int->Int->Bool
grade1 x y =  (fromIntegral y / fromIntegral x) < 0.5 

grade2::Int ->Int->Bool
grade2 x y =  ((fromIntegral y / fromIntegral x) >=0.5) && ((fromIntegral y / fromIntegral x) <0.63)

grade3::Int ->Int->Bool
grade3 x y =  ((fromIntegral y / fromIntegral x) >=0.63) && ((fromIntegral y / fromIntegral x) <0.76)

grade4::Int ->Int->Bool
grade4 x y =  ((fromIntegral y / fromIntegral x) >=0.76) && ((fromIntegral y / fromIntegral x) <0.89)

grade5::Int ->Int->Bool
grade5 x y =  ((fromIntegral y / fromIntegral x) >=0.89) 

grade6::Int -> Int ->Bool
grade6 x y = div (y*100) x<50

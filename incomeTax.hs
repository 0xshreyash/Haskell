incomeTax::Double -> Double
incomeTax x 
    | x < 3200.0 = 0.0
    | x <= 20000.0 = rest*0.25
    | otherwise = 20000*0.25 + (rest - 20000)*0.40
                   where rest = x - 3200

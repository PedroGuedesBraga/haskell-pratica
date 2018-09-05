divTuple (x, 0) = undefined

divTuple (x, y) = x/y


somatorio a b = sum [a .. b]


somatorioRec a b 
    | (b - a == 1) = a + b
    | otherwise = a + somatorioRec (a + 1) b

square x = x*x

sumSquares x y = square x + square y

retornaTrue = True

highOrderSum f a b = sum (map f [a, b]) 

hoSumSquares a b = highOrderSum square a b

mapFilter f p xs = filter p (map f xs)

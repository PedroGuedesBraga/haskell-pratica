--Exemplos de expressoes lambda
square = \x -> x*x

{-Potencia-}
{-Com "acucar sintatico"-}
pow = \x y -> x^y

{-Fatorial-}
fatorial = \x -> if(x /= 1) then x * fatorial(x-1) else 1

{-Verifica se é primo-}
isPrime = \x -> if (length [y | y <- [1..x], (x `mod` y) == 0] <= 2) then True else False

{-Fibonacci-}
fib = \x -> if (x == 1) then 1 else if (x == 2) then 1 else fib (x-1) + fib (x-2)

{-MDC-}
{-Com acucar sintatico-}
mdc = \a b -> if(b==0) then a else mdc b (a `mod` b)
{-Sem acucar sintatico de haskell-}
mdc2 = \a -> (\b -> if(b==0) then a else mdc b (a `mod` b))

{-mmc-}
mmc = \x y -> ((x*y)`div`(mdc x y))


{-coprimos-}
coprimos = \x y -> if (length (divisoresIguais x y) == 1 && head (divisoresIguais x y) == 1) then True else False

{-auxiliar: retorna uma lista com os divisores iguais, dado dois numeros-}
divisoresIguais = \x z -> if(x >= z) then [y | y <- [1..x], x `mod` y == 0, z `mod` y == 0] else [y | y <- [1..z], x `mod` y == 0, z `mod` y == 0]


{-*(ver) goldbach: Recebe um numero par maior que 2!-}
goldbach = \z -> [(x,y) | x <- getPrimes z, y <- getPrimes z, x + y == z, isPrime x, isPrime y]

{-auxiliar: retorna todos os numeros primos ate um numero-}
getPrimes = \x -> [y | y <- [1..x], isPrime y]

{-meuLast (usando recursao em expressoes lambda de uma forma mais simples-}
meuLast = \xs -> if(length xs > 1) then meuLast (tail xs) else if(length xs == 1) then head xs else error "Lista vazia!"

{-penultimo-}
penultimo = \xs -> if(length xs > 2) then penultimo (tail xs) else if (length xs == 2) then head xs else error "Lista sem penultimo!"

{-elementAt-}
elementAt = \p xs -> xs !! (p-1)
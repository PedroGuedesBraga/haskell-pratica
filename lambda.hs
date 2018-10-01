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

{-Auxiliar: - Sem açúcar sintatico-}
multSimple = \x -> \y -> x*y

greaterThanSimple :: Int -> Int -> Bool
greaterThanSimple = \x -> \y -> x > y

{-Auxiliar: - Com açúcar sintático-}
multSimple' = \x y -> x*y

greaterThanSimple' :: Int -> Int -> Bool
greaterThanSimple' = \x y -> x > y

{-Meu Length: Tamanho de uma lista (USANDO EXPRESSOES CASE)-}
meuLength :: [a] -> Int
meuLength = \xs -> case xs of [] -> 0
                              (x:y) -> 1 + meuLength (y)


{-Ver essa restricao "Eq a" adicionada na assinatura de tipos da funcao-}
meuLength' ::Eq a => [a] -> Int
meuLength' = \xs -> if (xs == []) then 0 else 1 + meuLength (tail xs)

{-Meu Reverso-}
meuReverso = \xs -> if (length xs == 0) then [] else (last xs):meuReverso (init xs)

{-Is Palindrome -> Ver a questão da assinatura da funcao / assinatura de tipos-}
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome = \xs -> xs == reverse xs

{-Compress: Remove elementos duplicados em uma lista-}
compress :: Eq a => [a] -> [a]
compress = \xs -> if(length xs == 0) then [] else if ((last xs) `elem` (init xs)) then compress (init xs) else compress(init xs) ++ ([last xs])

{-Encode: Retorna uma lista com os pares de elementos e suas quantidades-}
encode :: Eq a => [a] -> [(a, Int)]
encode = \xs -> if(length xs == 0) then [] else [(head xs, (returnQtd xs (head xs)))] ++ (encode (returnWithout xs (head xs)))  


{-Auxiliar: (Encode) Retorna a qtd de ocorrencias de um elem em uma lista-}
returnQtd :: Eq a => [a] -> a -> Int
returnQtd = \xs e -> length ([x | x <- xs, x==e])

{-Auxiliar: (Encode) retorna a lista sem um determinado elemento passado-}
returnWithout :: Eq a => [a] -> a -> [a]
returnWithout = \xs e -> [x | x <- xs, x/=e]

{-Split: Divide uma lista em duas sublistas, onde o ponto de divisão é dado-}
split = \xs i -> [take i xs, drop i xs]

{-InsertAt: Insere um elemento em uma determinada posicao da lista, a posicao nesse caso comeca do indice 0-}
insertAt = \el pos xs -> (take pos xs) ++ [el] ++ (drop pos xs)

{-mySum: Minha soma-}
mySum = \xs -> if(length xs == 0) then 0 else (head xs) + (mySum (tail xs))

maxList :: Ord a => [a] -> a
maxList = \xs -> if (length xs == 0 ) then undefined else if(length xs == 1) then head xs else (max (head xs) (maxList (tail xs)) )

{-myAppend-}
myAppend = \xs ys -> xs ++ ys
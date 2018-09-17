{- 
    Retorna o ultimo elemento em uma lista
-}

meuLast (x:[]) = x
meuLast (x:s) = meuLast s

{-
    Encontra o penúltimo elemento de uma lista
-}

penultimo (x:(s:([]))) = x
penultimo (x:s) = penultimo s

{-
    Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}

elementAt pos xs = xs !! (pos-1)

{-
    Retorna o tamanho de uma lista
-}

meuLength ([]) = 0
meuLength (x:xs) = 1 + meuLength xs

{-
    Retorna o inverso de uma lista
-}

meuReverso (x:[]) = [x]
meuReverso (x:xs) = (meuReverso xs) ++ [x]


{-
- Diz se uma lista é palindrome. 
-}

isPalindrome xs = xs == (meuReverso xs)

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}

{-Usar where para melhorar a legibilidade-}
compress xs
    | xs == [] = []
    | (last xs) `elem` init xs = compress (init xs)
    | otherwise = (compress (init xs)) ++ [(last xs)]
    

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}

compact [] = []
compact xs = (filter (==(head xs)) xs) ++ (compact (removeElem (head xs) xs))


{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}

split xs i = [[take i xs], [drop i xs]]

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}

slice xs imin imax = drop (imin - 1) (take (imax) xs)



{- Auxiliares-}

removeElem e [] = []
removeElem e (x:xs)
    | e == x = removeElem e xs
    | otherwise = x:(removeElem e xs)

 
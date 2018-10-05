{- tentar encontrar o primeiro elemento de uma lista satisfazendo um
predicado. Isso pode retornar alguém ou não -}

data Talvez a = Somente a | Nada deriving Show {-Para haskell conseguir fazer o 'print', o tipo deve fazer parte da typeclass Show-}

findElement p [] = Nada
findElement p (x:xs) = if (p x) then Somente x else findElement p xs 

isBiggerThan3 x = x > 3

{-Múltiplos construtores: Um tipo de dado/datatype pode ter múltiplos construtores de valores: Cada um deve ter uma declaracao diferente-}

{-Write a datatype Tuple which can hold one, two, three or
four elements, depending on the constructor (that is, there should be
four constructors, one for each number of arguments). Also provide
functions tuple1 through tuple4 which take a tuple and return Just the
value in that position, or Nothing if the number is invalid (i.e., you ask
for the tuple4 on a tuple holding only two elements).-}

{-Datatypes devem comecar sempre com letra maiuscula-}
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Show)

{-tuple 1/2/3/4 retornam no resultado, um valor do tipo Talvez // Recebem uma Tuple a b c d-}

tuple1 (Tuple1 a) = Somente a
tuple1 (Tuple2 a b) = Somente a
tuple1 (Tuple3 a b c) = Somente a
tuple1 (Tuple4 a b c d) = Somente a 


tuple2 (Tuple2 a b) = Somente b 
tuple2 (Tuple3 a b c) = Somente b 
tuple2 (Tuple4 a b c d) = Somente b
tuple2 _ = Nada

tuple3 (Tuple3 a b c) = Somente c 
tuple3 (Tuple4 a b c d) = Somente c
tuple3 _ = Nada


tuple4 (Tuple4 a b c d) = Somente d 
tuple4 _ = Nada


{-TIPOS DE DADOS RECURSIVOS-}

{-LISTAS-}
{-Pode ser Nada ou uma cabeca ligada a uma cauda (que é uma lista tbm-}
data Lista a = Vazia | Cons a (Lista a) deriving (Eq, Show) {-Tipo de dado recursivo, pode ser Nada ou definido em termos de si mesmo-}

{-Funcoes sobre listas-}
{-Tamanho (com pattern matching)-}
tamanho Vazia = 0
tamanho (Cons a as) = 1 + tamanho as

imprimeLista Vazia = []
imprimeLista (Cons a bc) = a:imprimeLista bc

cabeca (Cons a as) = a
cabeca (Vazia) = error "Nao ha cabeca para lista vazia"

cauda Vazia = error "Nao ha cauda para uma lista vazia"
cauda (Cons a as) = as 

ultimo Vazia = error "Nao ha ultimo para uma lista vazia"
ultimo (Cons a Vazia) = a 
ultimo (Cons a as) = ultimo as


{-BST-}

data BST a = NIL | Node a (BST a) (BST a) deriving (Show)

{-Tamanho de uma BST: Usando pattern matching-}
tamanhoBST NIL = 0
tamanhoBST (Node raiz esq dir) = 1 + tamanhoBST esq + tamanhoBST dir


{-Type: SINONIMOS - PERMITEM A CRIACAO DE NOVOS TIPOS A PARTIR DOS TIPOS EXISTENTES
-> A PALAVRA CHAVE: type: PERMITE DEFINIR NOVOS TIPOS A PARTIR DOS TIPOS EXISTENTES (SINONIMOS) - 
melhora a legibilidade-}
type IdCliente = Int

retornaId :: Int -> IdCliente
retornaId x = 3
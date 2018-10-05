--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c


--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente

--No lado direito, na definicao do tipo: "a" e "b" representam os diferentes tipos que podem ser usados 
--nos construtores de valores. Quadruple a b, significa que nos construtores de valores pode-se usar 
--2 tipos diferentes, a e b. a e b ainda podem ser do mesmo tipo sim.
data Quadruple a b = Quadruple a a b b deriving (Eq, Show)

firstTwo (Quadruple a b _ _) = [a,b]
secondTwo (Quadruple _ _ a b) = [a,b]

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq, Show)
data Talvez a = Nada | Somente a deriving (Eq, Show)

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

{-Listas-}
data List a = Nil | Cons a (List a) deriving (Eq,Show)

tamanhoLista Nil = 0
tamanhoLista (Cons x xs) = 1 + tamanhoLista xs

listHead Nil = error "Lista vazia nao tem head"
listHead (Cons x xs) = x

listTail Nil = error "Lista vazia nao tem tail"
listTail (Cons x xs) = xs

{-Aqui, usando pattern matching, a ordem importa-}
listLast Nil = error "Lista vazia nao tem ultimo elemento"
listLast (Cons x Nil) = x
listLast (Cons x xs) = listLast xs



--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show, Ord) --O fato de se usar a torna a binaryTree
--uma binaryTree de um só tipo (o tipo a). Haskell definira por inferencia de tipos

sizeBST NIL = 0
sizeBST (Node a b c) = 1 + sizeBST b + sizeBST c

--verifica se uma BT é uma BST
{-Usando pattern matching *( b é o filho a esq. e c é o filho a dir.-}
isBST (Node a NIL NIL) = True
isBST (Node a NIL c) = (a < getRoot c) && (isBST c)
isBST (Node a b NIL) = (a >= getRoot b) && (isBST b)
isBST (Node a b c) = (a >= getRoot b) && (a < getRoot c) && isBST(b) && isBST(c) 
--Auxiliares 
{-Pega a raiz de uma arvore/subarvore -}
getRoot (Node a b c) = a


--insere uma nova chave na BST retornando a BST modificada
--Recebe o elemento a ser adicionado e a BST na qual o elem sera adicionado
{-Usando pattern matching-}

{-Caso base: Inserir no lugar de um nó NIL/Sentinela-}
insert elem NIL = Node elem NIL NIL
{-Passo recursivo-}
insert elem (Node a b c) = if(a >= elem) then Node a (insert elem b) c else Node a b (insert elem c) 

search elem NIL = NIL
search elem (Node a b c) = if (a==elem) then Node a b c else if (a >= elem) then search elem b else search elem c 






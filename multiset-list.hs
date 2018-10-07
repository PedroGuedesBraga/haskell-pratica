module MultisetList ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}

bagInsert elem bag = if (bag == []) then [(elem, 1)] else if (fst (head bag) == elem) 
    then [(elem, (snd (head bag)) + 1)] ++ (tail bag) else [head bag] ++ bagInsert elem (tail bag) 


{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem bag = if (bag == []) then [] else if(fst (head bag) == elem) then removeDaTupla (head bag) ++ tail bag
    else [head bag] ++ remove elem (tail bag)

--decrementa da tupla
removeDaTupla tupla = if((snd tupla) <= 1) then [] else [(fst tupla, snd tupla - 1)] 

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem bag = if (bag == []) then 0 else if(fst(head bag) == elem) then snd (head bag) else search elem (tail bag) 

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}

--Coloca todos os elementos em uma unica bag    
unionBag bag1 bag2 = [x | x <- (bag1 ++ bag2), (elem x (union1 bag1 bag2)) || elem x (union1 bag2 bag1) ]

--Union1 retorna a uniao dos elementos que aparecem nas duas bags. Se o elemento aparecer apenas em bag2, ele nao sera adicionado na
--lista final. Isso é corrigido nos predicados usados em unionBag
union1 bag1 bag2 = if(bag1 == []) then [] else retornaMaior (head bag1) bag2 ++ (union1 (tail bag1) bag2)

retornaMaior tupla bag = if((search (fst tupla) bag) <= snd tupla) then [tupla] else [(fst tupla, (search (fst tupla) bag))]

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection bag1 bag2 = [x | x <- bag1++bag2, (elem (fst x) (getAllFirsts bag1)), (elem (fst x) (getAllFirsts bag2)), 
    (elem x bag1) && ((snd x) <= (search (fst x) bag2)) || (elem x bag2) && (snd x) <= (search (fst x) bag1)]

--AUXILIAR
getAllFirsts bag = if(bag == []) then [] else [fst (head bag)] ++ getAllFirsts (tail bag)





{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 bag2 = undefined

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
--otherBag == bag2: Testa se bag1 esta incluso em bag2
inclusion bag1 bag2 = length [x | x <- bag1, (snd x) <= (search (fst x) bag2)] == length bag1 

--pesquisa a quantidade de um elemento X em uma bag
searchQuantity x bag = if (bag == []) then 0 else if (fst (head bag) == x ) then snd (head bag) else searchQuantity x (tail bag)

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
--Todos os elementos que estao na soma de sumBag' + todos os elementos que estao apenas em bag2 (esses ultimos
-- nao foram considerados na soma de sumBag' :(  )
sumBag bag1 bag2 = [x | x <- (union (sumBag' bag1 bag2) (sumBag' bag2 bag1))] 

--Essa funcao abaixo nao considera elementos que aparecem somente em bag2, considera-se isso em sumbag
sumBag' bag1 bag2 = if(bag1 == []) then [] else sumBagAux (head bag1) (bag2) ++ sumBag' (tail bag1) bag2

--Soma uma tupla (elemento da bag) em uma bag, retornando uma lista com a tupla resultante da soma
sumBagAux tuple bag = if(bag == []) then [tuple] else if (fst (head bag) == fst tuple) 
    then [(fst tuple, snd (head bag) + snd tuple)] else sumBagAux tuple (tail bag)

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = if (bag == []) then 0 else (snd (head bag)) + size (tail bag)
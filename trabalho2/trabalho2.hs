-- Bruno Victor da Silva 11621BCC043
-- Yan Lucas Dias 11621BCC029
-- Trabalho 1 PF

l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]
{-
****Original****
bolha [] = []
bolha lista = bolhaOrd lista (length lista)

bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca [x] = [x]
troca (x:y:zs)
 |x>y = y :troca (x:zs)
 |otherwise = x :troca(y:zs)
 
-}
 
-- Variação 1: parada do algoritmo é antecipada
bolha1 :: Ord a=> [a]->([a],Int)
bolha1 [] = ([],0)   --lista vazia 0 trocas
bolha1 lista = bolhaOrd1 lista (length lista) 0 -- chama bolhaOrd com acumulador

bolhaOrd1 :: Ord a=> [a]->Int->Int->([a],Int) --lista tamanho acumulador ->()
bolhaOrd1 lista 0 _ = (lista,0)    --caso de parada iteração por tamanho
bolhaOrd1 lista n ac = (nova,ac_trocas+trocas)  --iteração por tamanho da lista
 where 
  (noval,ac_trocas) = troca1 lista n --percorre fazendo comparação em toda lista
  (nova,trocas) = bolhaOrd1 noval (n-1) ac_trocas  --(lista apos cada comparações, trocas por iteração)
  
troca1 :: Ord a=> [a]->Int->([a],Int)
troca1 [x] _ = ([x],0)    --caso de parada 1 elemento na lista
troca1 l 0 = (l,0)     --caso de parada da recursão sem trocas (PARADA ANTECIPADA)
troca1 (x:y:zs) n     --iteração na linha
 |x>y = (x1,ac1+1)    --elemento anterior é maior que o primeiro incrementa
 |otherwise = (x2,ac2)   --caso que não incrementa
 where
  (aux1,ac1) = troca1 (x:zs) (n-1)   --(lista ,acumulador de trocas) e itero
  x1 = y:aux1 --lista se x>y
  (aux2,ac2) = troca1 (y:zs) (n-1)  --(lista ,acumulador de trocas) e itero
  x2 = x:aux2 --lista se x<y
  
-- Variação 2: faz parada antecipada e, a cada iteração de trocas
bolha2 :: Ord a=> [a]->([a],Int)
bolha2 [] = ([],0)   --lista vazia 0 trocas
bolha2 lista = bolhaOrd2 lista (length lista) 0 -- chama bolhaOrd com acumulador

bolhaOrd2 :: Ord a=> [a]->Int->Int->([a],Int) --lista tamanho acumulador ->()
bolhaOrd2 lista 0 _ = (lista,0)    --caso de parada iteração por tamanho
bolhaOrd2 lista n ac = (nova,ac_trocas+trocas)  --iteração por tamanho da lista
 where 
  (noval,ac_trocas) = troca2 lista (n-1) --percorre fazendo comparação diminuindo o range de troca
  (nova,trocas) = bolhaOrd2 noval (n-1) ac_trocas  --(lista apos cada comparações, trocas por iteração) reduzindo tamanho da lista
  
troca2 :: Ord a=> [a]->Int->([a],Int)
troca2 [x] _ = ([x],0)    --caso de parada 1 elemento na lista
troca2 l 0 = (l,0)     --caso de parada da recursão sem trocas (PARADA ANTECIPADA)
troca2 (x:y:zs) n     --iteração na linha
 |x>y = (x1,ac1+1)    --elemento anterior é maior que o primeiro incrementa
 |otherwise = (x2,ac2)   --caso que não incrementa
 where
  (aux1,ac1) = troca2 (x:zs) (n-1)   --(lista ,acumulador de trocas) e itero
  x1 = y:aux1 --lista se x>y
  (aux2,ac2) = troca2 (y:zs) (n-1)  --(lista ,acumulador de trocas) e itero
  x2 = x:aux2 --lista se x<y

--1)
-- variacao 1 com contador - ordenou lista l1 com 0 trocas, l2 com 1999000,l3 com 2000 trocas 
-- l4 com 1999000, ja l5,l6,l7 demoraram muito tempo e não peguei esse valor
--x1 com 0, x2 com 190, x3 com 100, x4 com 90, x5 com 95, x6 com 66, x7 com 94
--
-- variacao 2 com contador - ordenou lista l1 com 0 trocas, l2 com 1999000,l3 com 2000 trocas 
-- l4 com 1999000, ja l5,l6,l7 demoraram muito tempo e não peguei esse valor
--x1 com 0, x2 com 190, x3 com 100, x4 com 90, x5 com 95, x6 com 66, x7 com 94
--
--2)
-- variacao 1 com contador - ordenar lista l1,l2,l3,l4 gastou aproximadamente 16s
--                                        l5,l6,l7 gastou mais de 3 min (n conclui o teste)
--										  de x1 a x7 aproximadamente 2 segundos
-- variacao 2 com contador - ordenar lista l1,l2,l3,l4 gastou aproximadamente 16s
--                                        l5,l6,l7 gastou mais de 3 min (n conclui o teste)
--										  de x1 a x7 aproximadamente 2 segundos
{-
****Original****
selecao:: (Ord a)=> [a]->[a]
selecao [] = []
selecao xs = [x]++selecao (remove x xs)
 where x = minimo xs

remove::(Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
 |a==x = xs
 |otherwise = x:(remove a xs)

minimo::(Ord a)=>[a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
 |x<= (minimo xs) = x
 |otherwise = minimo xs
-}
--Variação1: minimo e remove ocorre numa mesma função (remove_menor)
selecao1 :: [Integer]->([Integer],Integer)
selecao1 [] = ([],0)                  -- caso base lista vazia
selecao1 (x1:xs1) = (comparador:teste,ac1+ac2)
 where
  (l1,l2,comparador,ac1) = remove_menor xs1 x1  -- mando o tail da lista e o primeiro elemento
  nl = l1++l2
  (teste,ac2) = selecao1 nl
  
--										([menor,...],[maior,...],num a ser comparado, contador de comparação)
remove_menor :: [Integer] -> Integer -> ([Integer],[Integer],Integer,Integer)
remove_menor [] n = ([],[],n,0)     --caso base lista vazia
remove_menor (x:xs) n                   -- n é o numero a ser comparado
 | n < x = (nl1,[x]++nl2,comparador1,ac_trocas1)  -- percorre lista encontrando menor
 | otherwise = (nl3,[n]++nl4,comparador2,ac_trocas2+1) -- encontrado novo menor que n
 where
  (nl1,nl2,comparador1,ac_trocas1) = remove_menor xs n  --caso n<x
  (nl3,nl4,comparador2,ac_trocas2) = remove_menor xs x
-- Variação 2: Refaça a implementação do algoritmo Seleção usando funções genéricas (foldr ou foldr1).
selecao2 :: [Integer]->([Integer],Integer)
selecao2 [] = ([],0)                  -- caso base lista vazia
selecao2 (x1:xs1) = (comparador:teste,ac1+ac2)
 where
  (l1,l2,comparador,ac1) = remove_menor xs1 x1  -- mando o tail da lista e o primeiro elemento
  nl = l1++l2
  (teste,ac2) = selecao1 nl
  
--										([menor,...],[maior,...],num a ser comparado, contador de comparação)
remove_menor2 :: [Integer] -> Integer -> ([Integer],[Integer],Integer,Integer)
remove_menor2 [] n = ([],[],n,0)     --caso base lista vazia
remove_menor2 (x:xs) n                   -- n é o numero a ser comparado
 | foldr1 min (x:xs) == n = (nl1,[x]++nl2,comparador1,ac_trocas1)  -- foldr1 verifica se o menor elemento é n
 | otherwise = (nl3,[n]++nl4,comparador2,ac_trocas2+1) -- encontrado novo menor que n
 where
  (nl1,nl2,comparador1,ac_trocas1) = remove_menor xs n  --caso n<x
  (nl3,nl4,comparador2,ac_trocas2) = remove_menor xs x

--1)
-- variacao 1 com contador - ordenou lista l1 com 0 trocas, l2 com 1999000,l3 com 2000 trocas 
-- l4 com 1999000, l5 com 4002000,l6,l7 demoraram muito tempo e não peguei esse valor
--x1 com 0, x2 com 190, x3 com 100, x4 com 90, x5 com 95, x6 com 66, x7 com 94
--
-- variacao 2 com contador e função foldr1 - ordenou lista l1 com 0 trocas, l2 com 1999000,l3 com 2000 trocas 
-- l4 com 1999000, ja l5,l6,l7 demoraram muito tempo e não peguei esse valor
--x1 com 0, x2 com 190, x3 com 100, x4 com 90, x5 com 95, x6 com 66, x7 com 94
--
--2)
-- variacao 1 com contador - ordenar lista l1,l2,l3,l4 gastou aproximadamente 19s
--                                        l5,l6,l7 gastou 3 min e 10s aproximadamente
--										  de x1 a x7 aproximadamente 2 segundos
-- variacao 2 com foldr1 - ordenar lista l1,l2,l3,l4 gastou aproximadamente 25s
--                                        l5,l6,l7 gastou mais de 3 min (n conclui o teste)
--										  de x1 a x7 aproximadamente 2 segundos
{-
****Original****
insercao::(Ord a)=>[a]->[a]
insercao [] = []
insercao (x:xs) = insereOrd x (insercao xs)

insereOrd::(Ord a)=>a ->[a] ->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
 |x<=y = (x:y:ys)
 |otherwise = y:(insereOrd x ys)
-}
-- Variação 1: Refaça a implementação do algoritmo Inserção usando funções genéricas(foldr ou foldr1)
insercao :: [Integer]->([Integer],Integer)
insercao [] = ([],0)   --caso base lista vazia
insercao (x:xs) = (l,n)
 where
  (l,n) = foldr (insereOrd) ([x],0) xs
                            
 
insereOrd :: Integer->([Integer],Integer) ->([Integer],Integer)
insereOrd x ([],0) = ([x],0) 
insereOrd x ((y:ys),n)
 |x <= y = ((x:y:ys),n+1)          --elemento a ser inserido e menor que o primeiro
 |otherwise = (y:l,n+1)    --caso a ser inserido no meio da lista
 where
  (l,n) = insereOrd x (ys,0)
--1)
-- variacao 1 com contador - ordenou lista l1 com 1999 comparacoes, l2 com 1999,l3 com 2000 comparacoes 
-- l4 com 2000, l5,l6,l7 com 4000 comparacoes
-- de x1 a x7 com 19 comparacoes
--
--2)
-- variacao 1 com contador - ordenar lista l1 levou 1.5s, l2 levou 7s,l3 levou 1.5s, l4 levou 7s
-- l5,l6,l7 levou 14s de x1 a x7 aproximadamente 2 segundos
{-
****Original****
quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort[x|x<-xs,x<s]
                   ++[s]++
				   quicksort[x|x<-xs,x>=s]
-}
-- Variação 1: modifique o algoritmo original para que ao invés dos elementos maiores e
--menores serem encontrados com buscas independentes,
quicksort::(Ord a) => [a] -> ([a],[a],Integer)
quicksort [] = ([],[],0)
quicksort (x:xs) = divide x xs
 
divide :: Ord a => a->[a]->([a],[a],Integer)
divide _ []  = ([],[],0)
divide s (x:xs)                --s é a cabeça da lista (PIVÔ)
 |s<x = ([x]++l1,l2,c1+1)
 |otherwise = (l3,[x]++l4,c2+1)
 where
  (l1,l2,c1) = divide s xs
  (l3,l4,c2) = divide s xs

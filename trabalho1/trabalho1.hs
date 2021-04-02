-- Bruno Victor da Silva 11621BCC043
-- Yan Lucas Dias
-- Trabalho 1 PF

--1--

analisa_raizes :: Int -> Int -> Int -> String
analisa_raizes a b c
 |a==0 ="degenerada"
 |b^2 > 4*a*c = "possui duas raizes reais"
 |b^2 == 4*a*c = "possui uma raiz real"
 |otherwise = "nenhuma raiz real"

--2--

equacao :: Float->Float->Float->(Float,Float)
equacao a b c 
 |a /= 0 = (((-b)+(sqrt (delta a b c)))/(2*a),((-b)-(sqrt (delta a b c)))/(2*a))
 |otherwise = ((-c/b),a)
 
delta :: Float->Float->Float->Float
delta a b c = (b^2)-4*a*c

--3--

type Data = (Int , Int , Int)
preco :: Float->Data->Data->Float
preco x (d_atual,m_atual,a_atual) (dia,mes,ano)
 |(a_atual - ano) <= 2 && (m_atual <= mes) && (d_atual <= dia) = x*0.15
 |(a_atual - ano) <= 10 && (m_atual <= mes) && (d_atual <= dia) = x*0.4
 |(a_atual - ano) >= 70 && (m_atual <= mes) && (d_atual <= dia) = x*0.5
 |otherwise = x

--4--

--[x^3| x<-[1..20], even x, x>3 && x<11]
--[(m,n)| m<-[1..5], n<-[1..20],n>m && n<3*m]
--[x| l1<-[15,16], x<-[1..l1]]
--[x|x<-zip[2,4..10][3,5..11]]
--[x|x<-[5,9..21]]

--5--
--a

contaNegM2:: [Int]->Int
contaNegM2 x = length[n |n<-x, n > 1 && mod n 3 == 0]

--b

listaNegM2:: [Int]->[Int]
listaNegM2 x = [n |n<-x, n > 1 && mod n 3 == 0]

--6--

primos :: Int -> Int ->[Int]
primos x y = [z|z<-[x..y],primo z]

primo :: Int -> Bool    
primo x = case ((divisores x) == [1,x])of
        True->True
        False->False

divisores :: Int-> [Int] 
divisores x = [ y | y<-[1..x],mod x y ==0 ]

--7--

mmc :: Int->Int->Int->Int
mmc x y z = menorDivisor2 x (menorDivisor2 y z)

menorDivisor2 :: Int->Int->Int
menorDivisor2 x y = (x*y) `div` (mdc x y)

mdc :: Int->Int->Int
mdc a b 
    |(a<b)=mdc b a
    |(b==0)= a
    |otherwise = mdc b (a `mod` b)

--8--

cSerie:: Float->Int->Float
cSerie x n = sum ((map (/x) (map fromIntegral (impares n))) ++ (map (x/) (map fromIntegral (pares n))))

pares :: Int -> [Int]
pares n = [x|x<-[1..n],even x]

impares :: Int -> [Int]
impares n = [x|x<-[1..n],not (even x)]

--9--

fizzbuzz::Int->[String]
fizzbuzz n = [if mod x 2 == 0 then "FIZZ" else 
 if mod x 3 == 0 then "BUZZ" else 
 if mod x 3 == 0 && mod x 2 == 0 then "FIZZBUZZ" else
 if mod x 2 /= 0 || mod x 3 /= 0 then "No" else show x| x <- [1..n]]
 
--10--

sel_multiplos:: Int->[Int]->[Int]
sel_multiplos n l = [x|x<-l, mod x n == 0]

--19--

notas = [1,2,5,10,20,50,100]

notasTroco:: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco valor = [v:vs | v <- notas, valor >= v, vs <- notasTroco (valor-v) ]

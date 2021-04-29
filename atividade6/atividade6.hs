--1--
--a

teste :: Data ->Bool
teste (dia,mes,ano) 
 |dia>=1 && dia<=31 && (mes==1 || mes==3 || mes==5 || mes==7 || mes==8 || mes==10 || mes==12) = True
 |dia>=1 && dia<=30 && (mes==4 || mes==6 || mes==9 || mes==11) = True
 |dia>=1 && dia<=29 &&  mes==2 = True
 |otherwise = False

type Data = (Int , Int , Int) 
valida :: Data ->Bool
valida (dia,mes,ano)
 |verificar == True = True
 |otherwise = False
  where 
   verificar = teste(dia,mes,ano)
   
--b

bissexto2 :: Int -> Bool
bissexto2 x 
 |(x`mod`4==0 &&  not(x `mod` 100==0))||(x`mod`400==0)=True
 |otherwise = False
 
bissexto :: [Int] -> [Int]
bissexto [] = []
bissexto (x:xs)= if teste == True then x : bissexto xs else bissexto xs  
  where
   teste = bissexto2 x
   
--c

precede :: Data -> Data -> Bool
precede (dia1,mes1,ano1) (dia2,mes2,ano2)
    |(valida (dia1,mes1,ano1)&& valida(dia2,mes2,ano2))&&(ano1<ano2) = True
    |(valida (dia1,mes1,ano1)&& valida(dia2,mes2,ano2))&&((ano1==ano2)&&(mes1<mes2)) = True
    |(valida (dia1,mes1,ano1)&& valida(dia2,mes2,ano2))&&((ano1==ano2)&&(mes1==mes2)&&(dia1<dia2)) = True
    |otherwise = False


type Livro = (String , String , String , Int)
type Aluno = (String , String , String , Int)
type Emprestimo = (String , String , Data , Data , String )
type Emprestimos = [Emprestimo]

vencido::Emprestimo -> Data ->Bool
vencido (c_livro,c_aluno,data_emprestimo,data_devolucao,"aberto") (data_hoje) 
    | precede (data_hoje) (data_devolucao) = True
    | otherwise = False

vencido (c_livro,c_aluno,data_emprestimo,data_devolucao,"atrasado") (data_hoje) 
    | precede (data_hoje) (data_devolucao) = False
    | otherwise = True

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] data_hoje = []
atrasados (x:xs) data_hoje = if aberto == False then x: atrasados xs data_hoje else atrasados xs data_hoje
 where 
  aberto = vencido x data_hoje

bdEmprestimos :: Emprestimos
bdEmprestimos = [("H123C9","BSI945",(12,4,2021),(30,09,2021),"aberto"),
                 ("L433C5","BCC021",(01,4,2021),(10,06,2021),"atrasado"),
                 ("K333C9","BSI945",(1,3,2021),(30,06,2021),"aberto"),
                 ("P030C5","BCC095",(20,4,2021),(22,05,2021),"atrasado"),
                 ("M654C3","BCC008",(04,4,2021),(05,05,2021),"aberto")]

--d

passo :: (Int,Int)->(Int,Int)
passo (x,y)=(y,x+y)

fibo2 :: Int -> (Int,Int)
fibo2 0 =(0,1)
fibo2 n = passo(recursao)
 where
  recursao = fibo2(n-1)
  
--e

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if m > n then 0 else if m == n then m else (prodIntervalo (m+1) n) * m

fatorial :: Int -> Int
fatorial 0 = 1
fatorial a = x
 where
  x = prodIntervalo 1 a
  
--2--
--a

valida2 :: Data ->Bool
valida2 (dia,mes,ano) = let verificar = teste(dia,mes,ano)
                        in
                         if verificar == True then True else False
                         
--b  

bissexto' :: [Int] -> [Int]
bissexto' [] = []
bissexto' (x:xs) = let teste = bissexto2 x  
                   in if teste == True then x : bissexto' xs else bissexto' xs

--c

atrasados' :: Emprestimos -> Data -> Emprestimos
atrasados' [] data_hoje = []
atrasados' (x:xs) data_hoje = let aberto = vencido x data_hoje
                              in if aberto == False then x: atrasados xs data_hoje else atrasados xs data_hoje

--d

fibo2' :: Int -> (Int,Int)
fibo2' 0 =(0,1)
fibo2' n = let recursao = fibo2(n-1)
           in passo(recursao)

--e

fatorial' :: Int -> Int
fatorial' 0 = 1
fatorial' a = let x = prodIntervalo 1 a
             in x

--3--
--a
--(λ x.2*x+1)3
--(2*3+1)
--7

--b
--(λ x y.x-y)5 7
--(5-7)
--(-2)

--c
--(λ x y.x-y)
--(7-5)
--(2)

--d
--(λ x y.x-y) (λz.z/2)
--((z/2)-y)

--e
--(λ x y.x-y) ((λz.z/2)6)1
--(λ x y.x-y)(6/2) 1
--(3-1)
--(2)

--f
--(λ x.λ y. -x y)9 4
--(λx. (-x) 9)4
--((-4) 9)
--(-5)

--g
--(λ x.xx) (λ y.y)
--(yy)

--5--
--(\x y-> y)((\z->z)(\z->z))(\w->w)5
--((\f-> (\x-> f(f x))) (\y-> (y * y))) 3
--((\f-> (\x-> f(f x)))(\y->(y +y))) 5
--((\x-> (\y-> x +y) 5) ((\y-> y -3) 7))
--(((\f-> (\x-> f(f(f x)))) (\y-> (y * y))) 2)
--(\x-> \y-> x +((\x->x -3) y)) 5 6
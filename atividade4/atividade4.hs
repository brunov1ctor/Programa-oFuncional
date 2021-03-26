--2--
reverse[x|x<-[1..5]] ou [x|x<-[5,4..1]]
[x|x<-['a','c'..'e']]
[x|x<-[1,4..16]]
[x|x<-zip[1,-2,-5,-8,-11][1,5,9,13,17]]

--3--
--a
intervalo :: Int->Int->[Int]
intervalo a b 
 |a == b = []
 |otherwise = [x|x<-[a..b]]
 
--b
intervalo_par :: Int->Int->[Int]
intervalo_par a b 
 |a == b = []
 |otherwise = [x|x<-[a..b],even x]
 
--5--
quadrados :: Int->Int->[Int]
quadrados a b = [x^2|x<-[a..b]]

--6--
seleciona_impares :: [Int]->[Int]
seleciona_impares x = [ n | n<- x , odd n]

--7--
tabuada :: Int->[Int]
tabuada x =[y*x| y<-[1..10]] 

--8--
bissexto :: Int -> Bool
bissexto x 
    |(x`mod`4==0 &&  not(x `mod` 100==0))||(x`mod`400==0)=True
    |otherwise = False 
bissextos :: [Int]->[Int]
bissextos x = filter (bissexto) x

bissexto2 :: Data -> Bool
bissexto2 (dia,mes,ano) = bissexto ano
--9--
sublistas x =concat x

--10--
type Data = (Int , Int , Int)
valida :: Data ->Bool
valida (dia,mes,ano) 
    |dia>=1 && dia<=31 && (mes==1 || mes==3 || mes==5 || mes==7 || mes==8 || mes==10 || mes==12) = True
    |dia>=1 && dia<=30 && (mes==4 || mes==6 || mes==9 || mes==11) = True
    |dia>=1 && dia<=28 &&  mes==2 && not(bissexto2 (dia,mes,ano)) = True
    |dia>=1 && dia<=29 &&  mes==2 && bissexto2 (dia,mes,ano) = True
    |otherwise = False
  
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

e1::Emprestimo -> Data ->Bool
e1 (c_livro,c_aluno,data_emprestimo,data_devolucao,"aberto") (data_hoje) 
    | precede (data_hoje) (data_devolucao) = True
    | otherwise = False

e1 (c_livro,c_aluno,data_emprestimo,data_devolucao,"encerrado") (data_hoje) 
    | precede (data_hoje) (data_devolucao) = False
    | otherwise = True

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados x data_hoje = [y|y<-x,(e1 y data_hoje)]


bdEmprestimo :: Emprestimos
bdEmprestimo = [   ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
                    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
                    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

--11--
pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
else pertence a z
uniaoNRec:: Eq t => [t] -> [t] -> [t]
uniaoNRec [] l = l
uniaoNRec (x:xz) l = if pertence x l then uniaoNRec xz l
else x: uniaoNRec xz l


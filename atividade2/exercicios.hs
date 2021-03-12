dobro :: Float -> Float
dobro a = a * 2.0

quadruplo :: Float -> Float
quadruplo a = dobro(dobro a)

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt(a^2 + b^2)

calculaDistancia :: (Float, Float) -> (Float, Float) -> Float
calculaDistancia (a,b) (a1,b1) = hipotenusa (abs(a - a1)) (abs(b - b1)) 

ehPar :: Int -> Bool
ehPar a = if mod a 2 == 0 then True else False

ehImpar :: Int -> Bool
ehImpar a = not (ehPar a)

fToC :: Float -> Float
fToC a = (a - 32)/(5/9)

maior2 :: Int -> Int -> Int
maior2 a b = max a b

maior3 :: Int -> Int -> Int -> Int
maior3 a b c = maior2 a (maior2 a b)

ehposi :: Int -> Int
ehposi a 
 | a > 0 = 1
 | a < 0 = -1
 | otherwise = 0 


ehDigito :: Char -> Bool
ehDigito c = c >= 'a' && c <= 'Z'


ehtriangulo :: Int -> Int -> Int -> Bool
ehtriangulo a b c 
 | b - c < a && a < b + c = True
 | a - c < b && b < a + c = True
 | a - b < c && c < a + b = True
 | otherwise = False
 
conversao :: Float -> (Float,Float,Float)
conversao a = (a , a * 3.96 , a * 4.45)

type Data = (Int,Int,Int)

bissexto :: Int -> Bool
bissexto x | (mod x 400 == 0) = True
 | (mod x 4 == 0) && (mod x 100 /= 0) = True
 | otherwise = False
 
bissexto2 :: Data -> Bool
bissexto2 (d,m,a)
 | d >= 1 && d <= 31 && m >= 1 && m <= 12 && (bissexto a) = True
 | otherwise = False

valida :: Data -> Bool
valida (d,m,a)
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 ||
 m == 7 || m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 ||
 m == 11) = True
 | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
 | d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
 
precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2) 
 |a1 <= a2 && m1 <= m2 && d1 <= d2 = True
 |otherwise = False
 
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
em_dia :: Emprestimo -> Data ->Bool
em_dia (_,_,_,d2,_) d3 
 |precede d2 d3 = True
 |otherwise = False
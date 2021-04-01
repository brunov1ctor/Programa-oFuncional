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


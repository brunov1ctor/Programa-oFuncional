--1--
conta_ch:: [Char]->Int
conta_ch [] = 0
conta_ch (x:res) = 1 + conta_ch res

conta:: [t]->Int
conta [] = 0
conta (_:r) = 1+conta r

maior:: [Int] -> Int
maior [x] = x
maior (x:y:resto)
 |x>y = maior (x: resto)
 |otherwise = maior (y:resto)
 
primeiros:: Int-> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

pertence:: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True else pertence a z


uniaoR:: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniaoR xs l else x: uniaoR xs l

--2--

npares:: [Int] -> Int
npares [] = 0
npares (x:xs) = if (mod x 2 == 0) then 1 + npares xs else npares xs

--3--

produtorio:: [Int] -> Int
produtorio [x] = x 
produtorio (x:xs) = x * produtorio xs

--4--

comprime::[[a]]->[a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--5--

tamanho:: [t]->Int
tamanho [] = 0
tamanho (_:r) = 1+conta r

--6--
uniaoRec2:: Eq t => [t] -> [t] -> [t]
uniaoRec2 [] l = l
uniaoRec2 l (x:xs) = if pertence x l then uniaoR xs l else x: uniaoR xs l
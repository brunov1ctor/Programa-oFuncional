data Exp = 
    Val Int -- um numero
    | Add Exp Exp -- soma de duas expressoes
    | Sub Exp Exp -- subtração de duas expressoes
    | Mult Exp Exp -- multiplicacao de duas expressoes
    | Div Exp Exp -- divisao de duas expressoes
    
avalia :: Exp -> Int
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = div (avalia exp1) (avalia exp2)

-- (3+12)*(15-5)/(1*3)
a = (Div (Mult (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mult (Val 1) (Val 3)))

-- - ((6+8-5+1)*(2+6/2))
b = (Sub (Val 0) (Mult (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Div (Val 6) (Val 2)))))

data Jogada =
    Pedra | Papel | Tesoura
    deriving (Eq, Show)

vence :: Jogada -> Jogada -> Bool 
vence j1 j2 
    |j1 == Pedra && j2 == Tesoura = True 
    |j1 == Tesoura && j2 == Papel = True 
    |j1 == Papel && j2 == Pedra = True 
    |otherwise = False 

vencedor :: (Jogada, Jogada) -> Jogada
vencedor (j1,j2) = if vence j1 j2 then j1 else j2

vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras = map vencedor

data Nebuloso = Verdadeiro 
    | Falso 
    | Talvez Float 
    deriving (Show)

fuzzifica :: Float -> Nebuloso
fuzzifica x 
    |x <= 0 = Falso
    |x >= 1 = Verdadeiro
    |otherwise = Talvez x

verificaAlto :: Float -> Nebuloso
verificaAlto altura = fuzzifica y
    where y = (altura-1.70) / 0.20

verificaBarato :: Float -> Nebuloso
verificaBarato custo = fuzzifica y
    where y = (50.000-custo) / 20.000

data Estudande = 
    Colegio { 
        ano :: Int, 
        colegio :: String, 
        matricula :: String, 
        altura :: Float, 
        peso :: Float 
    }
    |Universitario { 
        universidade :: String, 
        curso :: String, 
        matricula :: String, 
        altura :: Float, 
        idade :: Int 
    }

alunos :: [Estudande]
alunos = [
    Colegio { ano = 1, colegio = "Nacional", matricula = "Nac11", altura = 1.55, peso = 55.0 },
    Colegio { ano = 3, colegio = "Nacional", matricula = "Nac12", altura = 1.95, peso = 90.0 },
    Colegio { ano = 2, colegio = "Nacional", matricula = "Nac13", altura = 1.87, peso = 87.0 },
    Colegio { ano = 5, colegio = "Gabarito", matricula = "Gab11", altura = 1.82, peso = 76.0 },
    Colegio { ano = 7, colegio = "Gabarito", matricula = "Gab12", altura = 1.75, peso = 67.0 },
    Colegio { ano = 1, colegio = "Gabarito", matricula = "Gab13", altura = 1.78, peso = 65.0 },
    Colegio { ano = 2, colegio = "Olimpo", matricula = "Oli11", altura = 1.88, peso = 95.0 },
    Colegio { ano = 8, colegio = "Olimpo", matricula = "Oli12", altura = 1.72, peso = 89.0 },
    Colegio { ano = 9, colegio = "Olimpo", matricula = "Oli13", altura = 1.45, peso = 50.0 },
    Colegio { ano = 3, colegio = "Olimpo", matricula = "Oli14", altura = 1.99, peso = 102.0 },
    Universitario { universidade = "UFU", curso = "Comp", matricula = "11111111", altura = 1.55, idade = 21 },
    Universitario { universidade = "UFU", curso = "Med", matricula = "11111112", altura = 1.95, idade = 22 },
    Universitario { universidade = "UFU", curso = "Eng", matricula = "11111113", altura = 1.87, idade = 25 },
    Universitario { universidade = "UNITRI", curso = "Comp", matricula = "11111114", altura = 1.82, idade = 22 },
    Universitario { universidade = "UNITRI", curso = "Med", matricula = "11111115", altura = 1.75, idade = 19 },
    Universitario { universidade = "UNITRI", curso = "Eng", matricula = "11111116", altura = 1.78, idade = 18 },
    Universitario { universidade = "UNITRI", curso = "Comp", matricula = "11111117", altura = 1.88, idade = 24 },
    Universitario { universidade = "UNA", curso = "Comp", matricula = "11111118", altura = 1.72, idade = 23 },
    Universitario { universidade = "UNA", curso = "Med", matricula = "11111119", altura = 1.45, idade = 20 },
    Universitario { universidade = "UNA", curso = "Eng", matricula = "11111121", altura = 1.99, idade = 19 }
    ]

verificaEstudante :: Estudande -> (String, Nebuloso)
verificaEstudante a = (matricula a, verificaAlto (altura a))

descobreAltos :: [Estudande] -> [(String, Nebuloso)]
descobreAltos = map verificaEstudante

data ArvoreBinInt = Nulo | 
                    No Int ArvoreBinInt ArvoreBinInt
                    deriving Show

arvEx = (No 2 (No 7 (No 2 Nulo Nulo)
                    (No 6 (No 5 Nulo Nulo) 
                          (No 11 Nulo Nulo)))
               (No 5 Nulo 
                     (No 9 (No 4 Nulo Nulo) 
                            Nulo)))

-- Exemplo Percursos

emOrdem :: ArvoreBinInt -> [Int]
emOrdem Nulo = []
emOrdem (No x esq dir) = emOrdem esq ++ [x] ++ emOrdem dir

preOrdem :: ArvoreBinInt -> [Int]
preOrdem Nulo = []
preOrdem (No x esq dir) = [x] ++ preOrdem esq ++ preOrdem dir

posOrdem :: ArvoreBinInt -> [Int]
posOrdem Nulo = []
posOrdem (No x esq dir) = posOrdem esq ++ posOrdem dir ++ [x]

-- Exercicios

folhas :: ArvoreBinInt -> [Int]
folhas Nulo = []
folhas (No x Nulo Nulo) = [x]
folhas (No x esq dir) = folhas esq ++ folhas dir

nosInternos :: ArvoreBinInt -> [Int]
nosInternos Nulo = []
nosInternos (No _ Nulo Nulo) = []
nosInternos (No x esq dir) = nosInternos esq ++ nosInternos dir ++ [x]

somaNosInternos :: ArvoreBinInt -> Int
somaNosInternos a = sum (nosInternos a)

pertence :: Int -> ArvoreBinInt -> Bool 
pertence valor arvore = valor `elem` emOrdem arvore

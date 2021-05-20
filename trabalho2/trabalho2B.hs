data Exp = 
    Val Int -- um numero
    | Add Exp Exp -- soma de duas expressoes
    | Sub Exp Exp -- subtração de duas expressoes
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
vencedoras [x] = [vencedor x]
vencedoras (x:xs) = vencedor x : vencedoras xs
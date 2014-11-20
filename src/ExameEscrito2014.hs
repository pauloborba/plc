module ExameEscrito2014 where

type Id = String

data ExpB = V | F | NEG ExpB | OU ExpB ExpB | VarB Id | AplB Operador ExpI 

type Operador = String
data ExpI = Lit Int | VarI Id | Apl Operador ExpI  

data Comando =   Atr Id ExpI 
               | If ExpB Comando Comando
               | While ExpB Comando
               | Seq Comando Comando

data Valor = Num Int | Bool Bool | Erro | Fun (Valor -> Valor)             


intEB a V s = Bool True
intEB a F s = Bool False
intEB a (NEG e) s = nott (intEB a e s)
intEB a (OU e f) s = orr (intEB a e s) (intEB a f s)
intEB a (VarB i) s = search i s
intEB a (AplB o e) s = apl (search o a) (intEI a e s)

search i [] = Bool False
search i ((j,v):l) = if i == j then v else search i l  

nott (Bool b) = Bool (not b)
nott _ = Erro

orr (Bool b) (Bool c) = Bool (b || c)
orr _ _ = Erro


intEI a (Lit d) s = Num d
intEI a (VarI i) s = search i s
intEI a (Apl o e) s = apl (search o a) (intEI a e s)

apl (Fun f) v = f v
apl _ _ = Erro


int a (Atr i e) s = let v = intEI a e s in (v,wr (i,v) s)
int a (If c c1 c2) s | (intEB a c s) == (Bool True) = int a c1 s
                     | (intEB a c s) == (Bool False) = int a c2 s
                     | otherwise = (Erro,s)
int a (While c c1) s | (intEB a c s) == (Bool True) = let (v,s1) = int a c1 s in int a (While c c1) s1 
                     | (intEB a c s) == (Bool False) = (Num 0,s) 
                     | otherwise = (Erro,s)
int a (Seq c1 c2) s = int a c2 s1
                      where (v,s1) = int a c1 s


wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)


instance Eq Valor where
   (Num d) == (Num e) = d == e
   (Bool b) == (Bool c) = b == c
   Erro == Erro = True
   _ == _ = False

p1 = Seq (Atr "x" (Lit 5)) (Atr "y" (Apl "fat" (VarI "x")))
p2 = Seq p1 (If (AplB "odd" (VarI "y")) (Atr "z" (VarI "x")) (Atr "z" (VarI "y")))
p3 = Seq p2 (While (AplB ">0" (VarI "z")) (Seq (Atr "x" (Apl "+2" (VarI "x"))) (Atr "z" (Apl "m10" (VarI "z")))))
p4 = Seq p3 (Atr "w" (VarI "x"))

amb = [("fat",Fun fat),("odd",Fun oddd),(">0",Fun m0),("+2",Fun m2),("m10",Fun m10)]
res = int amb p4 []
resv = fst res
ress = snd res

fat (Num d) = Num (fatorial d)
fat _ = Erro
fatorial 0 = 1
fatorial n = n * (fatorial (n-1))

oddd (Num d) = Bool (odd d)
oddd _ = Erro

m0 (Num d) = Bool (d > 0)
m0 _ = Erro

m2 (Num d) = Num (d+2)
m2 _ = Erro

m10 (Num d) = Num (d-10)
m10 _ = Erro

instance Show Valor where
    show (Num d) = show d
    show (Bool b) = show b
    show Erro = "Erro" 
    show (Fun f) = "Fun def"     
    
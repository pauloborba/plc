module ExameEscrito2015 where

-- Questão 1

-- get :: Eq a => a -> [(a, Bool)] -> Bool
get i [] = False
get i ((j,v):l) = if i == j then v else get i l 

-- gett :: (Eq a1, Num a) => a1 -> [(a1, a)] -> a
gett i [] = 0
gett i ((j,v):l) = if i == j then v else gett i l

-- g :: (a1 -> a -> a) -> [a] -> [[a1] -> a]
-- foldr :: (a -> b -> b) -> b -> [a] -> b
g = map.foldr



-- Questão 2

--get2 :: (Eq a, Fail b) => a -> [(a,b)] -> b
get2 i [] = erro
get2 i ((j,v):l) = if i == j then v else get2 i l  

class Fail t where
  erro :: t
  
instance Fail Double where
  erro = 0.0  

instance Fail Bool where
  erro = False  

instance Fail Valor where
  erro = Erro
  
instance Fail Result where
  erro = Falha  

data Valor = Erro | Num Double

data Result = Falha  

instance Show Valor where
   show Erro = "Erro"
   show (Num x) = show x

x = get2 "a" [("a",1::Double)]
y = get2 "a" [("b",Num (2::Double))]
z = get2 "a" [("a",Num (1::Double))]



-- Questão 3

data Termo = Variavel String | Literal Double | Binaria Op Termo Termo | 
             Unaria Op Termo | Atribuicao String Termo | Sequencia Termo Termo
data Op = Soma | Subtracao | Divisao | Negativo

-- vars :: (Num t, Num t1) => Termo -> [(String, t, t1)]
vars (Variavel s) = [(s,1,0)]
vars (Literal d) = []
vars (Atribuicao v e) = add [(v,0,1)] (vars e)
vars (Sequencia e f) = add (vars e) (vars f)
vars (Binaria op e f) = add (vars e) (vars f)
vars (Unaria op f) = (vars f)

add [] l = l
add ((v,r,w):vrws) l = addTriple (v,r,w) (add vrws l)

addTriple (v,r,w) [] = [(v,r,w)]
addTriple (v,r,w) ((v1,r1,w1):vrws) | v == v1 = addTriple (v,r+r1,w+w1) vrws
                                    | otherwise = (v1,r1,w1):(addTriple (v,r,w) vrws)

a = Atribuicao "x" (Literal 2)
b = Atribuicao "y" (Variavel "z")
c = Binaria Soma a b
d = Sequencia c (Atribuicao "z" (Variavel "y"))
res = vars d


-- Questão 4

data Termo1 = Variavel1 String | Literal1 Double | Binaria1 OpB Termo1 Termo1 | 
              Unaria1 OpU Termo1 | Atribuicao1 String Termo1 | Sequencia1 Termo1 Termo1
data OpB = Soma1 | Subtracao1 | Divisao1
data OpU = Negativo1

naoinicializadas t = fst(naoini t []) 

-- aux na prova
naoini (Variavel1 s) e = if (isIn s e) then ([],e) else ([s],e)      
naoini (Literal1 d) e = ([],e)
naoini (Atribuicao1 v t) e = (ni,v:e1) where (ni,e1) = naoini t e
naoini (Sequencia1 t u) e = (ni1 ++ ni2,e2) where (ni1,e1) = naoini t e
                                                  (ni2,e2) = naoini u e1
naoini (Binaria1 op t u) e = naoini (Sequencia1 t u) e
naoini (Unaria1 op t) e = naoini t e

isIn x [] = False
isIn x (y:ys) = (x == y) || isIn x ys

a1 = Atribuicao1 "x" (Literal1 2)
b1 = Atribuicao1 "y" (Variavel1 "z")
c1 = Binaria1 Soma1 a1 b1
d1 = Sequencia1 c1 (Atribuicao1 "z" (Variavel1 "y"))
e1 = Sequencia1 d1 (Atribuicao1 "q" (Variavel1 "p"))
res1 = naoinicializadas e1

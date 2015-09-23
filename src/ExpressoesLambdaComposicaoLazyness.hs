module ExpressoesLambdaComposicaoLazyness where

import FuncoesDeAltaOrdemFilterFold
import FuncoesDeAltaOrdemAplicacaoParcialMap

-- Relembrando foldr e seu tipo...
-- e a função global vista até agora (mediafr), compondo as menores...

-- Mas varremos a lista duas vezes… Se eficiência for um problema,
-- podemos varrer só uma vez? Como em Java? Recursão diferente, acumulando
-- valores como parâmetros. Recursão direta.

mediaaux s t [] = s/t
mediaaux s t (e:l) = mediaaux (s+e) (t+1) l

mediao l = mediaaux 0 0 l


-- Até agora, com frequência, repetimos o parâmetro em apenas um local 
-- do lado direito, na extremidade da chamada de outra função. Podemos
-- evitar isso omitindo o parâmetro e definindo uma função diretamente
-- em termos de outra...

somafrp = foldr (+) 0 
sizefrp = foldr soma1 0


-- Dizemos que a aplicação da função foldr é parcial… Não fornecemos
-- todos os parâmetros… Notem os tipos somafrp :: [Integer] -> Integer
-- A aplicação parcial de foldr retorna uma função como resultado. Na
-- verdade, ao invés de ver foldr como uma função que tem 3 parâmetros,
-- podemos ver como uma função que tem um parâmetro e retorna uma outra
-- função, que tem um parâmetro também e recebe outra função que recebe uma
-- lista e retorna um elemento de algum tipo.


-- Funções também podem ser representadas por expressões lambda. Valor
-- que denota uma função anônima. Similar a Java, mas bem mais limpo.
-- sizefrpl :: [a] -> Integer

sizefrpl = foldr (\x y -> 1 + y) 0


mediafold [] = []
mediafold l = s/t 
     where (s,t) = foldr (\x (s1,t1) -> (s+s1,t1+1)) (0,0) l


-- Podemos compor as partes de forma mais elegante, com composição de funções…
-- (.) f g x = f (g x)
-- (.) :: (b -> c) -> (a -> b) -> a -> c


-- Compondo as definições que criamos até agora…
-- principal :: [(a, Double, Double)] -> Double

principal = mediafr . (map snd) . aprovadosf . mediasPLC


-- Testando

teste = principal turma


-- Avaliação lazy, só avalia o que for necessário para a execução da função.
-- Normalmente, em outras linguagens, a avaliação é estrita (não lazy), e da 
-- esquerda para a direita.

lazy x y | x > 0 = x
         | otherwise = y

loop x = loop x

res5 = lazy 5 (loop 1)
resloop1 = lazy (loop 1) 5
resloop2 = lazy 0 (loop 1)

llazy [] = []
llazy (e:l) | e > 0 = [e]
            | otherwise = llazy l

res1 = llazy [-100..]


-- Funções como valores e resultados…
-- twice :: (a -> a) -> a -> a
-- Diferente de a -> a -> a -> a 

twice f = f . f


-- Exercício

avaliar (x,'+',y) = x + y
avaliar (x,'-',y) = x - y
avaliar (x,'*',y) = x * y
avaliar (x,'/',y) = x / y

av (x,op,y) = avOp op x y

avOp '+' = (+)
avOp '-' = (-) 
avOp '*' = (*) 
avOp '/' = (/) 

avaliarExpressoes [] = []
avaliarExpressoes (e : l) = avaliar e : avaliarExpressoes l

ae = map av

somaTotal l = sum (avaliarExpressoes (filtraDivisaoPorZero l))

st = sum . ae . fz

filtraDivisaoPorZero [] = []
filtraDivisaoPorZero ((x,'/',0) : l) = filtraDivisaoPorZero l
filtraDivisaoPorZero (e : l) = e : filtraDivisaoPorZero l

fz = filter (\(x,op,y) -> not(op == '/' && y == 0))

testea = [(4,'+',3),(4,'/',3)]
testeb = [(4,'+',3),(4,'/',0),(4,'/',3)]


-- Quais os tipos das funções abaixo?
-- f :: (a -> Bool) -> [[a]] -> [[a]]
-- g :: (a1 -> a -> a) -> [a] -> [[a1] -> a]

f = map.filter

g = map.foldr


-- Escopo estático e dinâmico... closure!

scope x y = g
      where g w = x + y + w

res17 = x + (scope 2 3) 3
      where x = 9

res20 = x + (scope 5 3) 3
      where x = 9

module InferenciaDeTiposPolimorfismoCompreensaoDeListasEstado where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao

    
-- Polimorfismo, várias funções com o mesmo nome, assumindo várias formas
-- Diversos tipos de polimorfismo (de subtipo, ad hoc, paramétrico)
-- Inferência de tipos, entender o mecanismo com zipp

zipp :: [t] -> [u] -> [(t,u)]
zipp (a:as) (b:bs) = (a,b):zipp as bs
zipp _ _ = []

-- Comparar com um zipp sem os underlines
-- Exercitar mecanismo de inferência de tipos com reverse e ++, e depois com o procurar.

-- Exercícios

removeDups [] = []
removeDups (e:l) = if member e l then removeDups l else e:removeDups l

unique [] = []
unique [e] = [e]
unique (x:y:ys) | x == y = unique (y:ys)
		    | x /= y = x:unique (y:ys)


-- Ao invés de recursão, podemos usar compreensão de listas.

removerc n l = [(n1,s) | (n1,s) <- l, n1 /= n ]

-- Exercício: definir as outras funções (cadastrar e existe não fazem muito sentido).

procurarc n l = [(n1,s) | (n1,s) <- l, n == n1 ]

creditarc n v l = [ if (n == n1) then creditarConta (n1,s) v else (n1,s) | (n1,s) <- l]

creditarcc n v l = [ (n1,s) | (n1,s) <- l, n1 /= n ] ++ [ creditarConta (n1,s) v | (n1,s) <- l, n1 == n ]

-- de qualquer forma, esses dois creditar não são equivalentes ao primeiro quando 
-- a lista tem mais de um par com o mesmo número
-- equivalência comportamental, semântica: duas funções são equivalentes quando, para todas
-- as possíveis entradas, elas retornam o mesmo resultado. contrastar com igualdade sintática.

qsort [] = []
qsort (e:l) =    qsort [x | x <- l, x < e ]
              ++ [e]
              ++ qsort [x | x <- l, x >= e ]

-- qsort mais eficiente com split

split p [] = ([],[])
split p (x:xs) = if (x >= p) then (l,x:r) else (x:l,r)
			where (l,r) = split p xs

eqsort [] = []
eqsort (x:xs) =  eqsort l ++ [x] ++ eqsort r
			where (l,r) = split x xs 


-- como simular estado com o banco...
-- onde a lista de contas ficaria armazenada? 
-- função que receberia lista infinita de strings correspondendo
-- a opções do menu e valores digitados, repassando o estado como parâmetro:

first (e:es) = e
second (e:es) = first es

sistema entrada = sist entrada []
sist [] estado = estado
sist ((comando,args):es) estado | comando == "cadastrar" =
                                  sist es (cadastrar (first args,0) estado)
                                | comando == "creditar" =
                                  sist es (creditar (first args) (read (second args)) estado)
                                | comando == "debitar" =
                                  sist es (debitar (first args) (read (second args)) estado)

-- Testando

est = sistema [("cadastrar",["1"]),("cadastrar",["2"]),("creditar",["1","55"]),
               ("creditar",["2","88"]),("debitar",["1","50"])]

-- e com concorrência?
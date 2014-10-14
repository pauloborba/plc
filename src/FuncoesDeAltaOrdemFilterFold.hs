module FuncoesDeAltaOrdemFilterFold where

import IntroducaoAProgramacaoFuncionalEHaskell
import FuncoesDeAltaOrdemAplicacaoParcialMap
    
-- Relembrar a definição de map, e exemplos de funções definidas em termos de map.
-- Calcular tipos das funções. Estudar os vários exemplos e exercícios nos slides.

-- Map com compreensão de conjuntos. Fica mais claro que todos os elementos da lista
-- vão ser transformados pela função f, e que a lista resultante tem o mesmo tamanho
-- da original.

mapc f l = [f e | e <- l]


-- Agora vamos selecionar só os aprovados… Precisamos fazer algo em todos
-- os elementos, mas nem todos são mantidos na lista resultante… Então map 
-- serve para essa situação!

aprovadosr [] = []
aprovadosr ((nome,nota) : l) | nota >= 7 = (nome,nota) : aprovadosr l
		             | otherwise = aprovadosr l


-- Ou, de forma mais concisa, com compreensão de listas…

aprovados l = [ (nome,nota) | (nome,nota) <- l , nota >= 7 ] 


-- Reprovados poderiam ser obtidos usando a operação de diferença entre listas,
-- ou então como abaixo, que teria que ser duplicado para, por exemplo, identificar
-- quem ficou abaixo de 5...

reprovadosr [] = []
reprovadosr ((nome,nota) : l) | nota < 7 = (nome,nota) : reprovadosr l
			      | otherwise = reprovadosr l

reprovados l = [ (nome,nota) | (nome,nota) <- l , nota < 7 ] 


-- Nos dois casos, estamos selecionando, filtrando, elementos de uma lista com base
-- na nota… O que muda é apenas o filtro, a função, o predicado, de seleção dos
-- elementos…

alunosr p [] = []
alunosr p ((nome,nota) : l) | p nota =  (nome,nota) : alunosr p l
			    | otherwise = alunosr p l

alunos p l = [(nome,nota) | (nome,nota) <- l , p nota]


-- Podemos então definir as duas funções sem duplicar a lógica de filtragem. Essa
-- lógica poderia ser reusada para implementar várias funções similares, e poderia ser
-- ajustada ou otimizada uma única vez, t

aprovadosa l = alunos maior7 l
maior7 nota = nota >= 7

reprovadosa l = alunos menor7 l
menor7 nota = nota < 7


-- alunos :: (t1 -> Bool) -> [(t, t1)] -> [(t, t1)]
-- Podemos generalizar ainda mais, não só filtrando baseado na nota, mas
-- no elemento como um todo...

mfilter p l = [x | x <-l , (p x)]

mfilterr p [] = []
mfilterr p (e : l) | p e = e : mfilterr p l
		        | otherwise = mfilterr p l

reprovadosf l = filter menorpar7 l
menorpar7 (nome,nota) = nota < 7

aprovadosf l = filter maiorpar7 l
maiorpar7 (nome,nota) = nota >= 7


-- mfilter :: (t -> Bool) -> [t] -> [t]
-- filter :: (a -> Bool) -> [a] -> [a]
-- aprovadosf :: (Ord a, Num a) => [(t, a)] -> [(t, a)]


-- Testando…

teste2 = aprovadosf teste1
teste3 = reprovadosf teste1



-- Agora, como calculamos a nota média dos aprovados? Assumindo que, com map,
-- conseguiremos facilmente só pegar as notas...

mediaTurma l = soma l / tamanho l


-- Testando…

teste4 = map snd teste2
teste5 = mediaTurma teste4 


-- Mas note que soma e tamanho têm um padrão de recursão bem parecido… Elas
-- aplicam a mesma operação aos elementos de uma lista… Desenhar árvore da lista
-- e a transformação que ocorre na árvore...

fold op []  = 0
fold op (e : l) = op e (fold op l)

somaf l = fold (+) l 
sizef l = fold soma1 l

soma1 x y = 1 + y

mediaf l = somaf l / sizef l


-- Fold um pouco diferente do que aparece nos slides; senão não daria para
-- representar tamanho…

-- fold :: Num a => (t -> a -> a) -> [t] -> a
-- Dá para generalizar ainda mais… Fold transforma listas em números, mas
-- podemos transformar listas em qualquer coisa...

foldrr op i []  = i
foldrr op i (e : l) = op e (foldrr op i l)


somafr l = foldr (+) 0 l 
sizefr l = foldr soma1 0 l

mediafr l = somafr l / sizefr l


-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldrr :: (t -> t1 -> t1) -> t1 -> [t] -> t1

nomess l = foldr separador "" l
concat l = foldr (++) [] l
and l = foldr (&&) True l

separador x y = x ++ "; " ++ y
              

-- Testando…

teste6 = mediafr teste4
teste7 = nomess (map fst teste2)



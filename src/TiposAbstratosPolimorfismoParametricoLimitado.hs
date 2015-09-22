module TiposAbstratosPolimorfismoParametricoLimitado where

import ExpressoesLambdaComposicaoLazyness

-- Voltando agora para o exercício de expressões de ExpressoesLambdaComposicaoLazyness, 
-- a representação de expressões como triplas não é tão adequada (representa expressões 
-- binárias, não expressões em geral), e é extremamente limitada (subexpressões têm que 
-- ser números, não podemos ter subexpressões complexas). Podemos resolver com definições 
-- de novos tipos, com novos elementos, em Haskell. Isso é diferente de definir sinônimos 
-- para tipos cujos valores já existem em Haskell.
--
-- Além de tipos primitivos, tuplas e listas, podemos ter estruturas definidas pelos
-- programadores, fazendo uso de união (lista é caso particular) de conjuntos de elementos
-- de estruturas diferentes (cada elemento é precedidos por um construtor, e é formado por
-- diferentes componentes).

type Operador = Char
data Expressao = Numero Float
               | Unaria Operador Expressao
               | Binaria Expressao Operador Expressao  


-- Equivalente em Java a classe abstrata (ou interface) e três subclasses.
-- Podemos então definir funções por casamento de padrão, assim como feito com listas.

avaliard (Numero f) = f
avaliard (Binaria e1 op e2) = avOp op (avaliard e1) (avaliard e2)
avaliard (Unaria op e) | op == '-' =  -(avaliard e)
                       | op == '+' =  avaliard e

showExpressao (Numero f) = show f
showExpressao (Binaria e1 op e2) = "("++ (showExpressao e1) ++ (showOp op) ++ 
                                         (showExpressao e2) ++ ")"
showExpressao (Unaria op e) = (showOp op) ++ (showExpressao e)

showOp '+' = " + "
showOp '-' = " - "
showOp '*' = " * "
showOp '/' = " / "


-- Testando

exp7 = Binaria (Numero 4) '+' (Binaria (Numero 6) '/' (Numero 2)) 
res7 = avaliard exp7
priexp7 = showExpressao exp7


-- Os construtores de tipo são vistos como funções que constroem elementos do tipo.
-- Numero :: Float -> Expressao
-- Binaria :: Expressao -> Operador -> Expressao -> Expressao
-- Unaria :: Operador -> Expressao -> Expressao
--
-- avaliard :: Expressao -> Float


-- Melhorando com datatype para operador. Reduz a quantidade de elementos do tipo criado.
-- Deixa de fora elementos como "Binaria (Numero 5.1) ';' (Numero 5.2)", que não tinham
-- significado para o problema em questão, mas mesmo assim faziam parte do conjunto de 
-- elementos do tipo. Em particular, a função avaliard não estava definida para todos os
-- elementos do seu tipo. O que aconteceria se a expressão binária acima fosse passada como
-- parâmetro? Agora poderia-se garantir que todos os casos estão sendo cobertos... Deixa 
-- de fora vários caracteres que não representavam operadores. Diminui a quantidade de 
-- elementos do tipo; deixa só os representativos.

data OpBinario = Soma | Sub | Mul | Div 
data OpUnario = Pos | Neg
data Exp = Literal Float
         | Un OpUnario Exp
         | Bin Exp OpBinario Exp  


-- Tipo definido pode ser polimórfico, parametrizado por tipo dos elementos literais.
-- Expp funciona então como um gerador de tipos; para cada tipo passado como parâmetro
-- para Expp, temos um novo tipo.

data Expp t = Litp t
            | Unp OpUnario (Expp t)
            | Binp (Expp t) OpBinario (Expp t)  

type ExpInt = Expp Int
type ExpStr = Expp String
type ExpFloat = Expp Float

exps = (Litp "4")
expf = (Litp 4.0)


-- Litp :: t -> Expp t
-- Unp :: OpUnario -> Expp t -> Expp t
-- Binp :: Expp t -> OpBinario -> Expp t -> Expp t


-- A função de avaliação precisa então receber como parâmetro uma função
-- que sabe transformar os literais em valores, um avaliador de literais,
-- já que podemos agora trabalhar com literais de diferentes tipos.

eval lit (Litp f) = lit f
eval lit (Binp e1 op e2) = evBOp op (eval lit e1) (eval lit e2)
eval lit (Unp Neg e) =  -(eval lit e)
eval lit (Unp Pos e) =  eval lit e

evBOp Soma = (+)
evBOp Sub = (-) 
evBOp Mul = (*) 
evBOp Div = (/) 


-- Observando o tipo de eval, vemos que o avaliador de literais tem
-- que retornar um elemento de um tipo "a", mas não um "a" qualquer, um
-- "a" que seja Fractional, um subtipo de Num que aceita divisão (/). 
--
-- eval :: Fractional a => (t -> a) -> Expp t -> a
-- evBOp :: Fractional a => OpBinario -> a -> a -> a


-- Para expressões com literais que são números, o avaliador de literais
-- é simplesmente a função identidade. 

exp8 = Binp (Litp 5) Soma (Binp (Litp 6) Div (Litp 2)) 
res8 = eval (\x -> x) exp8


-- Para expressões com literais que são strings, o avaliador de literais
-- é uma função que transforma strings em números. 

exp9 = Binp (Litp "6") Soma (Binp (Litp "6") Div (Litp "2")) 
res9 = eval (\x -> (read x)) exp9


-- Tipos existentes em Haskell poderiam ter sido definidos assim... e até outros...
-- Quais as diferenças entre os elementos dos três tipos de árvores definidos?

data List t = Nil | Cons t (List t)
data Tree t = NilT | Node t (Tree t) (Tree t)
data TTree t = Leaf t | NNode (TTree t) (TTree t)
data TTTree t = NilTTT | NNNode t [(TTTree t)] 


-- Algo similar ao tipo de eval ocorre com o tipo de == e show
--
-- (==) :: Eq a => a -> a -> Bool
-- show :: Show a => a -> String
--
-- Elas não funcionam para um tipo a qualquer. Funcionam para qualquer
-- tipo a que satisfaz algumas propriedades, que contém algumas funções
-- específicas. A função não serve apenas para um tipo específico (função
-- não polimórfica). Nem é completamente genérica, servindo para qualquer 
-- tipo (polimorfismo paramétrico ). É um meio termo, a função serve para 
-- um subconjunto dos tipos da linguagem, uma classe de tipos, o parâmetro 
-- tem que satisfazer algumas condições (bounded parametric polymorphism).
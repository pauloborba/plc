module IntroducaoAProgramacaoFuncionalEHaskell where

import Data.Char

-- fatorial, sintaxe da matemática, sem muita redundância
-- if statement in haskell
-- then obrigatório
-- parênteses opcional na condição

fat n = if n == 0 then 1 else n * fat (n-1)


-- comentários com -- ou {- -}
-- respondendo perguntas de sintaxe com o google
-- or grammar: http://www.haskell.org/onlinereport/haskell2010/
-- f n-1 é equivalente a (f n) - 1, se f só recebe um argumento

fatinfinito n = if n == 0 then 1 else n * fatinfinito n-1


-- casamento de padrão e recursão, tipos fortes, mas com inferência de tipos

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)


-- tipos básicos
-- Int operators: +, *, -, ^, div, mod :: Int -> Int -> Int
--                >, >=, ==, /=, <=, < :: Int -> Int -> Bool
-- Actually works for Num, to be discussed later, with type classes
-- Olhar http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1160006

max :: Int -> Int -> Int
max a b = if (a > b) then a else b


-- True, False :: Bool
-- &&, || :: Bool -> Bool -> Bool
-- not :: Bool -> Bool

xor a b = (a || b) && not(a && b)

axor True a = not a
axor False a = a


-- ’a’,’b’,...:: Char
-- ’\t’, ’\n’, ’\\’, ’\’’, ’\"’ :: Char
-- ord :: Char -> Int
-- chr :: Int -> Char
-- bem similar a Java

offset = ord 'A' - ord 'a'

maiuscula :: Char -> Char
maiuscula ch = if (ch >= 'a' && ch <= 'z') then
                    chr (ord ch + offset)
               else ch


-- String
-- ++ :: String -> String -> String
-- show :: ? -> String (overloading)


-- Float e Double
-- +,-,*,/ :: Float -> Float -> Float
-- pi :: Float
-- ceiling, floor, round :: Float -> Int
-- fromIntegral :: Int -> Float
-- read :: String -> Float
-- show :: Float -> String


-- Tuplas, agregando valores

addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

shift ((x,y),z) = (x,(y,z))


-- Nomeando tipos

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)

name :: Person -> Name
name (n,a,p) = n

type Saldo = Double
type Conta = (String,Saldo)

creditarConta (n,s) v = (n,s + v)
debitarConta (n,s) v = (n,s - v)


-- Listas
-- Ao contrário de Java, sintaxe concreta específica para listas.
-- type String = [Char]
-- [] e :
-- toda lista ou é vazia ou contém uma cabeça (elemento) e uma cauda (outra lista) 
-- toda lista finita acaba com a lista vazia
-- sintaxe equivalente: [12,3,4] e 12:3:4:[]  
-- ordem e quantidade é relevante
-- parênteses no pattern matching de listas é obrigatório

soma [] = 0
soma (nota : l) = nota + soma l

-- avaliar passo a passo a soma, como máquina de reescrita


nossohead (x:xs) = x

-- sem outra equação, a avaliação de nossohead [] leva a 
-- *** Exception: AplicacaoParcial.hs:114:1-20: Non-exhaustive patterns in function nossohead
-- por conta de coisas como essa, noção de fortemente tipada não é 100% correta para Haskell. Similar a Java, mas um pouco melhor.

tamanho [] = 0
tamanho (nota : l) = 1 + tamanho l

procurarIndice [] i = []
procurarIndice (e:l) 0 = [e]
procurarIndice (e:l) i = procurarIndice l (i-1)

-- visão geral do interpretador e do ambiente

ex = tamanho [4,1,2,3]


-- equações de funcões diferentes intercaladas dá erro: Multiple declarations of f1
-- duas equações começando com f 0 ou f n dá warning de overllaping de padrões, não erro
-- a primeira equação é usada; a ordem importa
-- o mesmo ocorre quando a equação de f n aparece antes de f 0


f1 0 = 1
f1 n = n * f1 (n-1)

f2 0 = 0
f2 m = m * f2 (m-1)


-- contar elementos em uma lista

count e [] = 0
count e (x:xs) | e == x = 1 + count e xs
               | otherwise = count e xs

               
-- conjuntos: união, interseção, diferença

member e [] = False
member e (x:xs) | x == e = True
                | otherwise =  member e xs

                
-- reuso versus eficiência

outromember e l = (count e l) > 0                


union [] c =  c
union (x:xs) c | member x c = union xs c
               | otherwise = x:(union xs c)


-- simetria               
               
intersection [] c = []
intersection (x:xs) c | member x c = x:(intersection xs c)
                      | otherwise = intersection xs c
                      

-- underscore poderia ter sido usado acima também                      
-- observar as similaridades entre as recursões e diferenças nas bases
                      
diff [] _ = []
diff c [] = c
diff (x:xs) c | member x c = diff xs c
              | otherwise = x:(diff xs c)
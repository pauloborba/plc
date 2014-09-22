module IntroducaoAProgramacaoFuncionalEHaskell where

import Data.Char

-- fatorial, sintaxe da matemática, sem muita redundância
-- then obrigatório
-- parênteses opcional na condição
-- comentários com -- respondendo perguntas de sintaxe com o google
-- if statement in haskell
-- grammar: http://www.informatik.uni-freiburg.de/~thiemann/haskell/haskell98-report-html/syntax-iso.html

fat n = if n == 0 then 1 else n * fat (n-1)

fatinfinito n = if n == 0 then 1 else n * fatinfinito n-1


-- casamento de padrão e recursão, tipos fortes, mas com inferência de tipos

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)


-- tipos básicos
-- Int operators: +, *, -, ^, div, mod :: Int -> Int -> Int
--                >, >=, ==, /=, <=, < :: Int -> Int -> Bool
-- Actually works for Num, to be discussed later, with type classes
-- Referência
--

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

nossohead (x:xs) = x

-- *** Exception: AplicacaoParcial.hs:114:1-20: Non-exhaustive patterns in function nossohead
-- por conta de coisas como essa, noção de fortemente tipada não é 100% correta para Haskell. Similar a Java, mas um pouco melhor.

-- avaliar passo a passo a soma, como máquina de reescrita

tamanho [] = 0
tamanho (nota : l) = 1 + tamanho l

procurarIndice [] i = []
procurarIndice (e:l) 0 = [e]
procurarIndice (e:l) i = procurarIndice l (i-1)

-- visão geral do interpretador
-- usar google como fonte para tirar dúvidas sobre a sintaxe 

ex = tamanho [1,2,3]
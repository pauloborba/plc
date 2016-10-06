module Prova2016 where

import System.IO
import Control.Exception

csv = "Paulo,13:05,21,09\nLeopoldo,17:09,22,09\nCastor,12:06,20,09\nPaulo,x,29,d\nLeopoldo,eee,23,d\nPaulo,x,20,d\nPaulo,y,21,d\nLeopoldo,eee,29,d\nLeopoldo,eee,29,d\nCastor,eee,29,d"

-- Questão 2
-- a acumula os caracteres até encontrar c
split c a [] = [a]
split c a (x:xs) | x == c = a:split c "" xs
                 | otherwise = split c (a ++ [x]) xs

splitColumns = split ',' ""

-- extra
splitColumnsAll = map splitColumns
splitLines = split '\n'

teste1 = splitLines "" csv
teste2 = splitColumnsAll teste1

discardItems n = map (discard n)
discard n [] = []
discard 1 (x:xs) = xs
discard n (x:xs) = x:discard (n-1) xs

discardAllItems [] l = l
discardAllItems (x:xs) l = discardAllItems (map (\x -> x-1) xs) (discardItems x l)

teste3 = discardItems 2 teste2
teste4 = discardAllItems [2] teste2
teste5 = discardAllItems [2,4] teste2


-- Questão 3
-- qsort não exigido na prova

data ItemDoRelatorio = Aluno String Int [Int] | Dia Int Int [String]
data DiaDaSemana = Seg | Ter | Qua | Qui | Sex | Sab | Dom

instance Show ItemDoRelatorio where
  show (Aluno aluno numEntradas dias) = ">> " ++ aluno ++ " esteve presente no lab " ++ (show numEntradas) ++ " vezes, nos seguintes dias: " ++ (show (qsort dias)) ++ "\n"
  show (Dia dia numAlunos alunos) = "No dia " ++ (show dia) ++ " tivemos " ++ (show numAlunos) ++ " alunos no lab: " ++ (show (qsort alunos)) ++ "\n"

instance Eq ItemDoRelatorio where
  (Aluno a1 _ _) == (Aluno a2 _ _) = a1 == a2
  (Dia d1 _ _) == (Dia d2 _ _) = d1 == d2
  _ == _ = False

instance Ord ItemDoRelatorio where
  (Aluno a1 _ _) > (Aluno a2 _ _) = a1 > a2
  (Dia d1 _ _) > (Dia d2 _ _) = d1 > d2
  _ > _ = False
  a < b = (a /= b) && not (a > b)
  a <= b = (a < b) || (a == b)


-- Questão 4
relatorio a add [] = a
relatorio a add (x:xs) = relatorio (add x a) add xs

addAluno e [] = [Aluno (aluno e) 1 [dia e]]
addAluno e ((Aluno a n ds):xs) | ((aluno e) == a) && (not (member (dia e) ds)) = (Aluno a (n+1) ((dia e):ds)):xs
                               | ((aluno e) /= a) = (Aluno a n ds):addAluno e xs
                               | otherwise = (Aluno a n ds):xs

addDia e [] = [Dia (dia e) 1 [aluno e]]
addDia e ((Dia d numAlunos alunos):xs) | ((dia e) == d) && (not (member (aluno e) alunos)) = (Dia d (numAlunos+1) ((aluno e):alunos)):xs
                                       | ((dia e) /= d) = (Dia d numAlunos alunos):addDia e xs
                                       | otherwise = (Dia d numAlunos alunos):xs

relatorioPessoa = (relatorio [] addAluno) . processaCSV
relatorioDia = (relatorio [] addDia) . processaCSV

processaCSV = (map (split ',' "")) . (split '\n' "")

dia l = read (nth 3 l)
aluno = nth 1

member e [] = False
member e (x:xs) = (e == x) || member e xs

nth _ [] = "ERRO!"
nth 1 (x:xs) = x
nth n (x:xs) = nth (n-1) xs

teste6 = relatorioDia csv
teste7 = relatorioPessoa csv


-- extra, para outro formato de csv

addAlunog dia aluno e [] = [Aluno (aluno e) 1 [dia e]]
addAlunog dia aluno e ((Aluno a n ds):xs) | ((aluno e) == a) && (not (member (dia e) ds)) = (Aluno a (n+1) ((dia e):ds)):xs
                               | ((aluno e) /= a) = (Aluno a n ds):addAlunog dia aluno e xs
                               | otherwise = (Aluno a n ds):xs

addDiag dia aluno e [] = [Dia (dia e) 1 [aluno e]]
addDiag dia aluno e ((Dia d numAlunos alunos):xs) | ((dia e) == d) && (not (member (aluno e) alunos)) = (Dia d (numAlunos+1) ((aluno e):alunos)):xs
                                       | ((dia e) /= d) = (Dia d numAlunos alunos):addDiag dia aluno e xs
                                       | otherwise = (Dia d numAlunos alunos):xs

relatorioPessoag = qsort . (relatorio [] (addAlunog ddia aaluno)) . processaCSV
relatorioDiag = qsort . (relatorio [] (addDiag ddia aaluno)) . processaCSV

ddia l = read (datadia l)
aaluno = nth 8

datadia l = if (dia /= []) && (sodigitos dia)
            then if head dia == '0'
                 then tail dia
                 else dia
            else "32"
             where
               dia = intervalo 9 10 (nth 1 l)

sodigitos [] = True
sodigitos (x:xs) = (x >= '0' && x <= '9') && sodigitos xs

intervalo n m [] = []
intervalo 1 m (x:xs) | m >= 1 = x:intervalo 1 (m - 1) xs
                     | m < 1 = []
intervalo n m (x:xs) | m > n = intervalo (n - 1) (m - 1) xs

qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

main =
    bracket (openFile "Log.csv" ReadMode) hClose
            (\h -> do contents <- hGetContents h
                      putStrLn (show (relatorioPessoag contents))
                      putStrLn (show (relatorioDiag contents)))


-- extra

unique _ [] = []
unique a (x:xs) | member (nth 2 x) a = unique a xs
                | otherwise = x:unique ((nth 2 x):a) xs

memberItem _ [] = False
memberItem a ((Aluno aluno n ds):xs) = (a == aluno) || memberItem a xs

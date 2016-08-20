module InferenciaDeTiposPolimorfismoCompreensaoDeListasEstado where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao

-- listas de tópicos a explorar na aula

-- na aula passada...

qsort [] = []
qsort (e:l) =    qsort [x | x <- l, x < e ]
              ++ [e]
              ++ qsort [x | x <- l, x >= e ]


-- qsort mais eficiente com split

-- split com o mesmo problema de performance do primeiro quicksort

splitt p l = ([x | x <- l, x < p], [x | x <- l, x >= p])


-- split p l

-- eqsort l

-- como simular estado com o banco...
-- onde a lista de contas ficaria armazenada? 
-- função que receberia lista infinita de strings correspondendo
-- a opções do menu e valores digitados, repassando o estado como parâmetro

-- est = sistema [("cadastrar",["1"]), ("cadastrar",["2"]), ("creditar",["1","55"]),
-- ("creditar",["2","88"]), ("debitar",["1","50"])]

-- e com concorrência?

-- isso é continuação da aula passada, falta colocar aqui a nova aula
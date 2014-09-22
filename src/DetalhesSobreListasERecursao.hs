module DetalhesSobreListasERecursao where

import IntroducaoAProgramacaoFuncionalEHaskell

procIndice l i = case l of
                    [] -> []
                    (e:l) -> if (i == 0) then [e]
                             else procIndice l (i-1)


-- Testando

i1 = procurarIndice [12,3,4,5,9] 3
i2 = procurarIndice [12,3,4,5,9] 6
i3 = procIndice [12,3,4,5,9] 3
i4 = procIndice [12,3,4,5,9] 6

-- procurarP n [] = []
-- procurarP n ((n,s):cs) = [(n,s)]
-- variável não pode ser repetida no padrão...
-- procurarP n ((n1,s):cs) = procurarP n cs


-- Mais funções sobre listas, agora com contas

cadastrarBugado c l = l ++ [c]

procurar n [] = []
procurar n1 ((n2,s):l) | n1 == n2 = [(n2,s)]
                       | n1 /= n2 = procurar n1 l

remover n [] = []
remover n1 ((n2,s):l) | n1 == n2 = l
                      | n1 /= n2 = (n2,s) : remover n1 l

creditar n v [] = []
creditar n1 v ((n2,s):l) | n1 == n2 = (creditarConta (n2,s) v) : l
                         | n1 /= n2 = (n2,s) : (creditar n1 v l)

debitar n v [] = []
debitar n1 v ((n2,s):l) | n1 == n2 = (debitarConta (n2,s) v) : l
                        | n1 /= n2 = (n2,s) : (debitar n1 v l)

existe n l = (procurar n l) /= []

cadastrar c l = if (existe (fst c) l) then l else l ++ [c]


-- Testando

banco1 = []
banco2 = cadastrar ("12-3",100) (cadastrar ("45-6",0) (cadastrar ("12-3",0) banco1))
conta1 = procurar "12-3" banco2
banco3 = creditar "12-3" 55 banco2
conta2 = procurar "12-3" banco3
banco4 = remover "45-6" banco3
tamanhoBanco = tamanho banco4


-- Exercício: definir ++, ver tipo de ++
-- definir reverse
-- não precisa (rev l) ++ [e], nem e:(conc l m), aplicação de função puxa pela esquerda…

rev [] = []
rev (e:l) = rev l ++ [e]

conc [] l = l
conc (e:l) m = e:conc l m

-- Indo bem devagar, com tempo para eles pensarem e fazerem cada uma
-- das funções acima antes de eu explicar a minha solução…
-- Por isso a terceira aula foi até aqui.

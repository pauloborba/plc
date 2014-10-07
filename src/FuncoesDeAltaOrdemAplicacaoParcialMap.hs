module FuncoesDeAltaOrdemAplicacaoParcialMap where

-- Vamos explicar um bônus que daremos para quem conseguir
-- expressar a função do cálculo de bônus em Haskell...

mediaComBonus1 x y = if (x + y)/2 > 7 then
		      	(x + y)/2 + 0.5
                     else
			(x + y)/2


-- Código repetido, clones… problemas associados… Para resolver,
-- refactoring Extract function (vocês já devem ter feito isso antes!)
--
-- Processo de abstração, mas não ainda generalização… Deixa o código
-- mais legível, mas não mais geral.

media x y = (x + y)/2

mediaComBonus2 x y = if media x y > 7 then 
		      	media x y + 0.5
                     else
			media x y


-- Não só abstrair os detalhes da operação de cálculo da média, mas também
-- a realização da operação… Melhora legibilidade e performance, mas continua
-- no mesmo nível de generalidade de antes.

mediaComBonus3 x y = if m > 7 then
		        m + 0.5
                     else
		        m
                     where m = media x y


-- Para evitar a repetição do m, alguns podem preferir como abaixo; acho 
-- acima melhor mesmo. Questão de estilo, qualidade é, pelo menos 
-- parcialmente, subjetiva…

mediaComBonus4 x y = m + (if m > 7 then 0.5 else 0)
                     where m = media x y


-- Supondo que isso é parte do Sig@, professores vão querer manter o 
-- príncipio geral do bônus mas mudar um pouco os detalhes, como o 
-- limite da média...

mediaComBonus4v x y = m + (if m > 6 then 0.5 else 0)
                      where m = media x y


-- Assim, para evitar a duplicação de código acima (ruim, por exemplo,
-- quando quisermos dar pesos diferentes para as notas, ou mudar o valor 0.5),
-- podemos generalizar o limite da média para receber bônus, parametrizando
-- esse valor, ao invés de deixá-lo fixo.

mediaComBonus5 x y v = m + (if m > v then 0.5 else 0)
                       where m = media x y

mediaComBonus5a x y = mediaComBonus5 x y 7    -- equivalente a mediaComBonus4
mediaComBonus5b x y = mediaComBonus5 x y 6    -- equivalente a mediaComBonus4v


-- Caso necessário, o mesmo processo de generalização pode ser aplicado para 
-- o valor do bônus (e eventualmente o 0). Os princípios de bonificação são os
-- mesmos de antes, mas a política foi generalizada...

mediaComBonus6 x y v b = m + (if m > v then b else 0)
                         where m = media x y

mediaComBonus6a x y = mediaComBonus6 x y 7 0.5  -- equivalente a mediaComBonus5a
mediaComBonus6b x y = mediaComBonus6 x y 6 0.5  -- equivalente a mediaComBonus5a
mediaComBonus6c x y = mediaComBonus6 x y 6 0.3  -- nova alternativa, variação


-- Melhorando agora a legibilidade… Só generalizar se necessário, solicitado
-- por algum cliente. Por isso não generalizamos o 0, que poderia ser
-- uma penalização (-0.5, por exemplo)…

mediaComBonus7 nota1 nota2 limite bonus = 
	  mediaNotas + (if mediaNotas > limite then bonus else 0)
      where mediaNotas = media nota1 nota2

mediaPLC nota1 nota2 = mediaComBonus7 nota1 nota2 7 0.5
mediaMR nota1 nota2 = mediaComBonus7 nota1 nota2 6 0.5


-- Agora, como aplicamos uma política específica a todos os alunos
-- de uma turma? Supondo que temos um cadastro (lista) com nomes dos
-- alunos e notas das duas unidades… Calcular a média final dos alunos...

mediasPLC [] = []
mediasPLC ((nome,nota1,nota2) : l) = (nome,(mediaPLC nota1 nota2)) : mediasPLC l


-- Para outra turma, com outra política, uma alternativa seria duplicar código…

mediasMR [] = []
mediasMR ((nome,nota1,nota2) : l) = (nome,(mediaMR nota1 nota2)) : mediasMR l


-- Poderia resolver, generalizando limite e bônus…

mediaslb [] limite bonus = []
mediaslb ((nome,nota1,nota2) : l) limite bonus = 
         (nome,(mediaComBonus7 nota1 nota2 limite bonus)) : 
         mediaslb l limite bonus


-- Mas, para professores que não gostam de bônus, a solução não funcionaria
-- bem e teríamos que repetir código de novo…

medias [] = []
medias ((nome,nota1,nota2) : l) = (nome,(media nota1 nota2)) : medias l


-- O que fazer? Aplicar o mesmo tipo de generalização que evitou a duplicação
-- de código entre mediaPLC e mediaMR? Possível com funções? Sim, funções são
-- cidadãos de primeira classe!

-- mediasf :: (Float -> Float -> Float) -> [(String, Float, Float)] -> [(String, Float)]

mediasf f [] = []
mediasf f ((nome,nota1,nota2) : l) = (nome,(f nota1 nota2)) : mediasf f l

mediasPLCg l = mediasf mediaPLC l  
mediasMRg l = mediasf mediaMR l  
mediasg l = mediasf media l


-- Serve até para implementar outras políticas, diferente de cálculo
-- da média!

maiores l = mediasf maior l
maior nota1 nota2 = if (nota1 > nota2) then nota1 else nota2


-- Usando aplicação parcial
-- Toda função em Haskell é vista como uma função que recebe apenas um argumento
-- e possivelmente retorna uma função como resultado

mediaComBonus8 limite bonus nota1 nota2 = mediaComBonus7 nota1 nota2 limite bonus
mediaslbg l limite bonus = mediasf (mediaComBonus8 limite bonus) l


-- Todas aplicam uma função diferente às notas da tripla… Poderia ser feito em OO?
-- Sim! Como?


-- Polimorfismo, paramétrico, não de subtipo e ad hoc (vocês já devem ter 
-- visto isso antes)
-- medias :: Fractional t1 => [(t, t1, t1)] -> [(t, t1)]
-- mediasf :: (t -> t1 -> t3) -> [(t2, t, t1)] -> [(t2, t3)]


-- Todas aplicam uma função diferente às notas da tripla… Mas e se só queremos 
-- a lista de nomes dos alunos? 

nomes [] = []
nomes ((nome,nota1,nota2) : l) = nome : nomes l


-- O padrão de recursão é o mesmo… inclusive para só retornar a nota final… Em todos
-- os casos, estamos aplicando uma função (diferente para cada caso) a todos elementos da lista… 

mapr f [] = []
mapr f (e:l) = (f e) : mapr f l

nomet (nome,nota1,nota2) = nome
mediat (nome,nota1,nota2) = (nome,media nota1 nota2)
mediaPLCt (nome,nota1,nota2) = (nome,mediaPLC nota1 nota2)
mediaMRt (nome,nota1,nota2) = (nome,mediaMR nota1 nota2)
maiort (nome,nota1,nota2) = (nome,maior nota1 nota2)
nota1t (nome,nota1,nota2) = nota1
nota2t (nome,nota1,nota2) = nota2

nomesm l = map nomet l
mediasm l = map mediat l
mediasPLCm l = map mediaPLCt l
mediasMRm l = map mediaMRt l
maioresm l = map maiort l
notas1m l = map nota1t l



-- Funciona para diferentes tipos de funções nos elementos. Qual o tipo de map?
-- map :: (a -> b) -> [a] -> [b]
-- mediasPLCm :: (Ord t1, Fractional t1) => [(t, t1, t1)] -> [(t, t1)]
-- detalhes das classes só são explicados depois...


-- Testando…

turma = [("Pedro",9,3),("Mariana",7,7),("Paulo",5,5),("Clarissa",10,5)]
teste1 = mediasPLCm turma

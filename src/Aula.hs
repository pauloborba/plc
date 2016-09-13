module Aula where

-- listas de tópicos a explorar na aula
-- essencialmente cópia do arquivo da aula, sem o código a ser
-- desenvolvido on-the-fly, só os comentários, os nomes das funções
-- a serem desenvolvidas, e as definições das funções que só vão ser
-- discutidas ou explicadas, não desenvolvidas do zero

-- na aula passada...

-- Interpretadores recebem programas como entrada. Strings como
--
--   "def inc = (lambda x . + x 1.0);
--    def v = + 3.9 2.1;
--    def resultado = inc v"
--
-- são dadas como entrada, e a saída é o resultado da execução.
--
-- Para interpretar programas, precisamos representr os programas de forma
-- abstrata, como uma árvore resultante do processo de parsing. Para
-- representar os programas de uma linguagem funcional simples,
-- temos o seguinte (identificador Identifier, número Literal, lambda exp Lambda,
-- aplicação Aplicacao, definição Def, e programa).

type Id = String
data TermoLinFun =   Literal Double
                   | Identifier String
                   | Lambda String TermoLinFun
                   | Aplicacao TermoLinFun TermoLinFun
                   -- Aplicacao String TermoLinFun TermoLinFun

data Definicao = Def String TermoLinFun

-- Melhor como acima do que como acima
-- type Definicao = (String,TermoLinFun)

type Programa = [Definicao]


-- Por exemplo, o programa abaixo
--
-- def inc = (lambda x . + x 1); def v = + 3 2; def resultado = inc v
--
-- seria representado como

def1 = Def "inc" (Lambda "x" (Aplicacao (Aplicacao (Identifier "+") (Identifier "x")) (Literal 1)))
def2 = Def "v" (Aplicacao (Aplicacao (Identifier "+") (Literal 3)) (Literal 2))
def3 = Def "resultado" (Aplicacao (Identifier "inc") (Identifier "v"))
prog1 = [def1,def2,def3]


-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções.

data ValorFun = Numero Double
              | Funcao (ValorFun -> ValorFun)
              | Excecao

instance Show ValorFun where
    show (Numero n) = show n
    show (Funcao f) = "Function definition cannot be printed!"
    show Excecao = "Excecao durante a execucao do interpretador"


-- A função que implementa o interpretador dos termos precisa receber como parâmetro um
-- ambiente, contendo as funções pré-definidas, e as definidas pelo programador.

type Ambiente = [(Id,ValorFun)]

-- No nosso caso, o ambiente teria apenas a definição de "+".

ambientesimples = [("+",Funcao (\x -> (Funcao (\y -> somaValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao


-- Temos agora duas funções de interpretação, uma para termos e uma
-- para programas. A de termos simplesmente lê o ambiente. A de programa
-- propaga alterações no ambiente, para acumular as funções definidas.

intTermo a (Literal n) = Numero n
intTermo a (Identifier i) = getValor i a
intTermo a (Lambda i t) = Funcao (\x -> intTermo ((i,x):a) t)
intTermo a (Aplicacao t1 t2) = aplica v1 v2
                               where
                                 v1 = intTermo a t1
                                 v2 = intTermo a t2

intPrograma a [] = Excecao
intPrograma a [Def i t] = intTermo a t
intPrograma a ((Def i t):ds) = intPrograma ((i,intTermo a t):a) ds



getValor i [] = Excecao
getValor i ((j,v):l) = if i == j then v else getValor i l

aplica (Funcao f) v = f v
aplica _ _ = Excecao



-- Nas linguagens com atribuição (o valor de uma variável pode mudar ao
-- longo da execução), precisamos lidar com a noção de estado. Além do
-- ambiente contendo definições imutáveis, precisamos de uma noção de estado, ou
-- memória, para armazenar os valores das variáveis em um determinado ponto da
-- execução. A função de interpretação não só recebe o estado como parâmetro.
-- Ela também retorna como resultado o valor da interpretação e o novo estado,
-- contendo as alterações nos valores das variáveis.

-- Para simplifcar, temos apenas termos na linguagem, incluindo atribuições
-- (que podem representar tanto definições no sentido do interpretador anterior
-- quanto mudanças nos valores de variáveis) e composição sequencial (como o ";"
-- em Java, que entre outras coisas faz o papel da lista de definições da linguagem
-- anterior). Representamos, por simplicidade, soma como um termo específico da
-- linguagem.

type Numero = Double
data Termo = Var Id
           | Lit Numero
           | Som Termo Termo
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Id Termo
           | Seq Termo Termo

-- A aplicação "(lambda x . + x 2) 3" seria
termo1 = (Apl (Lam "x" (Som (Var "x") (Lit 2))) (Lit 3))

-- A aplicação "(lambda x . + x y) 3" seria
termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit 3))

-- A composição sequencial "y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
termo3 = (Seq (Atr "y" termo2) termo2)

-- A composição sequencial "y := 3 ; (lambda x . + x y) 3" seria
sq1 = (Seq (Atr "y" (Lit 3)) termo2)

-- A composição sequencial "y := 3 ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
sq2 = (Seq (Atr "y" (Lit 3)) termo3)

-- A composição sequencial "y := (z := 5) + z ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit 5)) (Var "z"))) termo3)



-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções. Como as funções
-- podem acessar e modificar variáveis que mudam de valor ao longo
-- da execução, é necessário receber não só o argumento da função,
-- e retornar seu resultado. É preciso receber também o estado atual,
-- e retornar o novo estado modificado pela execução da função.

data Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro

type Estado = [(Id,Valor)]


-- int :: [(Id, Valor)] -> Termo -> [(Id, Valor)] -> (Valor, [(Id, Valor)])
-- int :: Ambiente -> Termo -> Estado -> (Valor, Estado)
--

int a (Var x) e = Erro

-- search :: Eq a => a -> [(a, Valor)] -> Valor

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

-- somaVal :: Valor -> Valor -> Valor

somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro

-- app :: Valor -> Valor -> Estado -> (Valor, Estado)

app (Fun f) v e = f v e
app _ _ e = (Erro, e)

-- wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]

wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)


-- Chamando o interpretador com o ambiente e a memória vazios.

at t = int [] t []

-- Se soma não fosse um termo específico da linguagem:
-- at t = int [("+",Fun (\x -> \e -> (Fun (\y -> \e2 -> (somaVal x y,e2),e)))] t []

instance Show Valor where
   show (Num x) = show x
   show Erro = "Erro"
   show (Fun f) = "Função"

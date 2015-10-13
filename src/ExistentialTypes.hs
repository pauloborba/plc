{-# LANGUAGE ExistentialQuantification #-}
module ExistentialTypes where

-- Na linguagem anterior, só elementos de Double podiam ser usados como literais. 
-- Uma opção seria criar um construtor para cada tipo de literal, o que geraria uma
-- certa replicação do código de avaliação dos mesmos. Outra opção seria assumir que
-- literais são strings e aí, na avaliação dos mesmos, fazer o parsing da string para
-- então transformar no valor correto. Outra opção é estabelecer que o componente do
-- construtor Lit pode ser qualquer tipo que possa ser transformado em Valor.

type Id = String
data Termo =  Var Id
           | forall a . (Interpretable a) => Lit a
           | Som Termo Termo
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Id Termo
           | Seq Termo Termo


class Interpretable a where
    eval :: a -> Valor

instance Interpretable Double where
    eval n = Num n

instance Interpretable Bool where
    eval b = Boolean b

-- Note que a semântica é bem diferente do que se "a" fosse um parâmetro do tipo
-- Termo, como em data Termo a = ...
-- Nesse caso, teríamos um gerador de linguagem. Para cada instanciação de Termo,
-- uma nova linguagem com seu tipo específico de literal. Mas nos programas de 
-- uma dada linguagem não seria possível usar literais de tipos diferentes, como
-- Double e Bool, por exemplo.
    

data Valor = Num Double
           | Boolean Bool
           | Fun (Valor -> StateTransformer Valor) 
           | Erro


data StateTransformer a = ST (Estado -> (a,Estado))

instance Monad (StateTransformer) where
   return r = ST (\e -> (r,e))
   (ST m) >>= f = ST (\e -> let (v,e1) = m e
                                (ST n) = f v
                            in (n e1)
                     ) 

type Estado = [(Id,Valor)]

type Ambiente = [(Id,Valor)]


-- Na avaliação de literais, podemos então usar a função eval.
--int :: [(Id, Valor)] -> Termo -> StateTransformer Valor

int:: [(Id, Valor)] -> Termo -> StateTransformer Valor

int a (Var i) = ST (\e -> (search i (a++e),e))

int a (Lit n) = return (eval n)

int a (Som t u) = do t1 <- int a t
                     u1 <- int a u
                     return (somaVal t1 u1)

int a (Lam i t) = return (Fun (\v -> int ((i,v):a) t))

int a (Apl f t) = do f1 <- int a f
                     t1 <- int a t
                     app f1 t1

int a (Atr i t) = do v <- int a t
                     assign (i,v)

		 {- = ST (\e -> let (ST f) = int a t 
                                (v,ei) = f e
                            in (v,wr (i,v) ei))  
                  -}
		     
int a (Seq t u) = do int a t
                     int a u


assign (i,v) = ST (\e -> (v,wr (i,v) e))

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l  

somaVal (Num x) (Num y) = Num (x + y)
somaVal _ _ = Erro

app (Fun f) v = f v
app _ _ = return Erro

wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)

-- É preciso anotar os números com o tipo Double porque Haskell o número
-- 2 ou 2.0 como um Num ou um Fractional, não como Double. Como Double é
-- a única instância de Interpretable, ele poderia ter feito essa inferência.
-- Dizer também que Fractionable seria uma instância de Interpretable geraria
-- problemas em funções como somaVal, que exigem que os dois Nums contenham
-- elementos do mesmo tipo.

termo1 = (Apl (Lam "x" (Som (Var "x") (Lit (2::Double)))) (Lit (3::Double)))
termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit (3::Double)))
termo3 = (Seq (Atr "y" termo2) termo2)
sq1 = (Seq (Atr "y" (Lit (3::Double))) termo2)
sq2 = (Seq (Atr "y" (Lit (3::Double))) termo3)
sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit (5::Double))) (Var "z"))) termo3)
sq4 = (Seq (Atr "y" (Lit True)) (Seq (Atr "y" (Lit (3::Double))) termo2))
at t = let (ST m) = (int [] t)
       in m []

instance Show Valor where
   show (Num x) = show x
   show (Boolean x) = show x
   show Erro = "Erro"
   show (Fun f) = "Função"



-- Ao invés de interpretar as expressões e obter o resultado final das suas execuções, 
-- podemos também só analisar o programa para verificar propriedades mais simples, como
-- para onde flui informações sigilosas. Assim, não precisamos armazenar os valores das
-- variáveis, mas apenas a informação de se ela contém informação sigilosa ou não.
-- Temos também considerar o caso de falha, e da variável armazenar uma função. É 
-- necessário armazenar a função para depois saber se informação é vazada na aplicação 
-- dessa função.

data Result = B Bool | F ((Result,FlowState) -> (Result,FlowState)) | Falha

type FlowState = [(String,Result)]

instance Eq Result where
    (B x) == (B y) =  x == y
    Falha == Falha = True
    _ == _ = False

instance Show Result where
    show (B x) = show x
    show Falha = "Falha"
    show (F _) = "Função"
    

data TaintTransformer a = TT (FlowState -> (a,FlowState))

instance Monad (TaintTransformer) where
   return r = TT (\e -> (r,e))
   (TT m) >>= f = TT (\e -> let (v,e1) = m e
                                (TT n) = f v
                            in (n e1)
                     )    


-- adicionar if e while na linguagem

ana:: Termo -> TaintTransformer Result

ana (Var i) = TT (\tainted -> (get i tainted,tainted))

ana (Lit n) = return (B False)

ana (Som t u) = do t1 <- ana t
                   u1 <- ana u
                   return (orr t1 u1)

ana (Atr i t) = do v <- ana t
                   add i v 
                   return v

ana (Lam i t) = return (let (TT f) = ana t
                        in (F (\(v,tainted) -> removePair i (f (wr (i,v) tainted)))))                   

ana (Apl f t) = do f1 <- ana f
                   t1 <- ana t
                   appp f1 t1
		     
ana (Seq t u) = do ana t
                   ana u

appp (F f) r = TT (\tainted -> f (r,tainted)) 
appp _ _ = return Falha

orr (B x) (B y) = (B (x || y))
orr _ _ = Falha

add i v = TT (\t -> (v,wr (i,v) t))

removePair i (r,l) = (r,remove i l)

remove i l = [e | e <- l , i /= (fst e)]   

get i [] = Falha
get i ((j,v):l) = if i == j then v else get i l  

p1 = Atr "x" (Var "x")
p2 = Atr "y" (Var "x")
p3 = Atr "z" (Var "y")
p4 = Atr "w" (Lit (3::Double))
p5 = Atr "y" (Var "w")
p6 = Atr "a" (Lam "i" (Atr "y" (Var "i")))
p7 = Atr "b" (Apl (Var "a") (Var "x"))
p8 = Var "b"
p9 = Seq p1 (Seq p2 (Seq p3 (Seq p4 (Seq p5 (Seq p6 (Seq p7 p8))))))

-- assumindo caso onde, inicialmente, apenas "x" contém informação sigilosa.
-- resultado mostra que variáveis podem conter informação sigilosa no final
-- da execução, e se o resultado final gerado pelo programa vaza informação
-- sigilosa

analisador p = let (TT f) = ana p 
               in f [("x",(B True))]
module MaisPolimorfismoParametricoLimitadoClassesDeTipo where

import FuncoesDeAltaOrdemAplicacaoParcialMap
import TiposAbstratosPolimorfismoParametricoLimitado


-- verificar o tipo de mediaaux :: Fractional a => a -> a -> [a] -> a

-- Para termos o polimorfismo paramétrico limitado, temos que definir as restrições, 
-- ou as classes de tipos. Bem parecido com interfaces em Java.
--
-- Haskell já traz várias classes definidas, já que elas são usadas para definir
-- os tipos de vários operadores primitivos da linguagem. Veja a definição de 
-- algumas classes similares às pré-definidas em Haskell.

class Eqq t where
    (===) :: t -> t -> Bool

class  Showw a  where  
    showwsPrec        :: Int -> a -> ShowS  
    showw             :: a -> String  
    showwList         :: [a] -> ShowS  


-- Uma função que tem como parâmetro um tipo "a" que seja da classe Eqq (o que seria
-- representado como "Eqq a"), pode receber como argumento um elemento de qualquer 
-- tipo que tenha um operador "===" e tenha sido definido como uma instância de Eqq.
-- Depois veremos como definir instâncias de uma classe.
-- 
-- Função tem parâmetro do tipo Int, só recebe argumentos inteiros
-- Função tem parâmetro de tipo a qualquer, recebe argumento de qualquer tipo
-- Função tem parâmetro de tipo a que seja da classe Eqq, recebe argumento de qualquer 
--        tipo que seja instância de Eqq (consequentemente esse tipo tem um operador ===) 

 
-- São instâncias de Eq os tipos primitivos e as listas e tuplas de instâncias de Eq.
--
-- Mas e os novos tipos que definimos, como associamos às classes? 
-- E as novas classes, como associamos às instâncias (tipos) primitivas?
-- Se não associamos os novos tipos a Eq, não podemos usar ==
--
-- eqTypeError = Cons 5 Nil == Cons 6 Nil
-- No instance for (Eq (List t0)) arising from a use of `=='


-- Associação derivada, pode não ser o ideal, implicitamente define
-- operadores para um tipo existente.

data Liste t = Nile | Conse t (Liste t)
               deriving (Eq,Ord,Show)

eqTypeOK = Conse 5 Nile == Conse 6 Nile


-- Aplicação item a item das operações existentes para os tipos
-- primitivos. Não tão óbvio para Ord, que compara elementos...

sim = Conse 5 Nile <= Conse 6 Nile
nao = Conse 6 Nile <= Conse 5 Nile
naonao = Conse 6 Nile <= Conse 5 (Conse 3 Nile)
simsim = Conse 5 (Conse 3 Nile) <= Conse 6 Nile


-- Associação definida, operadores da classe são definidos de
-- forma explícita. Não precisa definir todos os operadores; alguns
-- são definidos indiretamente a partir de equações. No Caso de Show,
-- basta apenas definir show ou showwsPrec.
--
-- See the definitions, with equations, for the original classes in the Haskell prelude: 
-- https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009

instance Show Expressao where
  show = showExpressao


-- Sem isso, mandar avaliar exp7 no interpretador gera
-- No instance for (Show Expressao)
--      arising from a use of `print'


-- Poderia também ter definido a função, caso a caso, dentro do instance

instance Show OpBinario where
  show Soma = " + " 
  show Sub = " - " 
  show Mul = " * "
  show Div = " / "
 
instance Show OpUnario where
  show Pos = "+"
  show Neg = "-"


-- Instanciando com tipos polimórficos, restrições necessárias antes do =>
-- List t é uma instância de Eq para qualquer t que seja instância de Eq

instance Eq t => Eq (List t) where
   Nil == Nil = True
   (Cons e l) == (Cons f m) = (e == f) && l == m
   _ == _ = False


-- Para poder avaliar exp8 sem problema no interpretador, precisamos
-- da seguinte declaração. E funciona porque 
--           exp8 :: Expp Double
-- e Double é uma instância de Show.
   
instance Show t => Show (Expp t) where
    show (Litp t) = show t
    show (Unp op e) = show op ++ "(" ++ show e ++ ")"
    show (Binp e op f) =  "(" ++ show e ++ show op ++ show f ++ ")"


-- Classes também podem assumir restrições... Herança
-- de restrições... Como herança de interfaces em Java...
--
-- See the definitions, with equations, for the original classes in the Haskell prelude: 
-- https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009

class Eq t => Ordd t where
    (<<),(<<=),(>>),(>>>=) :: t -> t -> Bool
    maxx, minn :: t -> t -> t


-- A classe pode herdar várias restrições, assim como uma instância pode
-- impor várias restrições.

class  (Eqq a, Show a) => Numm a  where  
    (+), (-), (⋆)  :: a -> a -> a  
    negate         :: a -> a  
    abs, signum    :: a -> a  
    fromInteger    :: Integer -> a  
 
class  (Numm a, Ordd a) => Reall a  where  
    toRational ::  a -> Rational  
    
class  Enumm a  where  
    succ, pred     :: a -> a  
    toEnum         :: Int -> a  
    fromEnum       :: a -> Int  
    enumFrom       :: a -> [a]            -- [n..]  
    enumFromThen   :: a -> a -> [a]       -- [n,n'..]  
    enumFromTo     :: a -> a -> [a]       -- [n..m]  
    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]  
 
class  (Reall a, Enumm a) => Integrall a  where  
    quot, rem, div, mod :: a -> a -> a  
    quotRem, divMod     :: a -> a -> (a,a)  
    toInteger           :: a -> Integer  
 
class  (Numm a) => Fractionall a  where  
    (/)          :: a -> a -> a  
    recip        :: a -> a  
    fromRational :: Rational -> a  
 
class  (Fractionall a) => Floatingg a  where  
    pi                  :: a  
    exp, log, sqrt      :: a -> a  
    (⋆⋆), logBase       :: a -> a -> a  
    sin, cos, tan       :: a -> a  
    asin, acos, atan    :: a -> a  
    sinh, cosh, tanh    :: a -> a  
    asinh, acosh, atanh :: a -> a


-- Agora dá para entender o tipo de mediaPLC e mediasPLC...
--
-- mediaPLC :: (Ord a, Fractional a) => a -> a -> a
-- mediaPLC nota1 nota2 = mediaComBonus7 nota1 nota2 7 0.5
-- media x y = (x + y)/2
-- mediaComBonus7 nota1 nota2 limite bonus = mediaNotas + (if mediaNotas > limite then bonus else 0)
--      where mediaNotas = media nota1 nota2
--
-- mediasPLC [] = []
-- mediasPLC ((nome,nota1,nota2) : l) = (nome,(mediaPLC nota1 nota2)) : mediasPLC l 
-- mediasPLCm :: (Ord t1, Fractional t1) => [(t, t1, t1)] -> [(t, t1)]
-- 

class (Ord t, Fractional t) => Media t 

instance Media Float

mediaPLend :: Media t => t -> t -> t
mediaPLend x y =  mediaComBonus7 x y 7 0.5 

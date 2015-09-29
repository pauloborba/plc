module EntradaESaidaMonads where

import Data.Char

-- Monad é uma classe especial bastante usada em toda a linguagem, para
-- entrada e saída, coompreensão de listas, etc. 
--
-- Útil para simular estado em linguagens funcionais. IO, por exemplo, 
-- usado no Main deste projeto. Intuitivamente, para o I de IO, a função 
-- recebe String de entrada e produz resultado mais nova String de entrada, 
-- com primeiro elemento consumido.
--
-- Mas, antes de ver essa classe, vamos analisar como seria getChar e putChar 
-- sem monads.
--
-- Simulando entrada e saída como parâmetro e resultado de funções.

-- gC :: ([a], t) -> (a, ([a], t))

gC (e,s) = (head e, (tail e,s))

-- pC :: a -> (t, [a]) -> (t, [a]) 

pC c (e,s) = (e, s ++ [c])


-- Um main simples seria então algo da seguinte forma, recebendo
-- io como entrada e gerando io como saída.
--
-- Tem todo o trabalho de ficar repassando o novo io, artificialmente
-- usar pares como resultado em várias funções, muito where, etc.
--
-- m :: ([Char], [Char]) -> ([Char], [Char])

m io = pC (somac c1 c2) io2
       where (c1,io1) = gC io
             (c2,io2) = gC io1

-- ou

mm io = let (c1,io1) = gC io 
            (c2,io2) = gC io1
        in
            pC (somac c1 c2) io2 
             

-- Função soma simplificada, obviamente:

somac :: Char -> Char -> Char   
somac c1 c2 = '5'


-- Graças a monads, na prática, em Haskell, é feito assim:
-- mmm :: IO ()

mmm = do c1 <- getChar 
         c2 <- getChar
         putChar (somac c1 c2) 


-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- getLine :: IO String
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()


-- Usando uma notação especial que, na verdade, é uma abreviação
-- para o código abaixo, usando os operadores da class Monad!

mmmm = getChar >>= (\c1 ->  
       getChar >>= (\c2 ->
       putChar (somac c1 c2)))


-- IO é um monad, por isso pode usar a notação acima, já que todo monad
-- tem o operador >>=
--
-- class Monad m where 
--    return :: a -> m a 
--    >>= :: m a -> (a -> m b) -> m b
--
-- No caso específico de IO, esse operador faz toda a transição de estado de
-- entrada e saída, como veremos depois.

-- Vantagem de se encaixar na classe Monad é que pode usar notação do do;
-- mostrar ela e a alternativa...

t1a = getChar >>= (\c -> putChar c)
t1b = do c <- getChar
         putChar c

t2a = getChar >>= (\c -> putChar c >>= (\d -> putChar c))
t2b = do c <- getChar
         putChar c
         putChar c

t3a = getChar >>= (\c -> putChar c >>= (\d -> getChar >>= (\e -> putChar e)))

t3b = do c <- getChar
         putChar c
         e <- getChar 
         putChar e

t3c = getChar >>= (\c -> 
      putChar c >>= (\d -> 
      getChar >>= (\e -> 
      putChar e)))

t3d = do c <- getChar
         d <- putChar c
         e <- getChar 
         putChar e


-- Monads, um tipo especial de classe, que possibilita uso de sintaxe 
-- especial em Haskell, como vemos abaixo em notação alternativa à do
-- prelude.

class Monadd m where 
    init :: a -> m a 
    bind :: m a -> (a -> m b) -> m b
    -- return e >>=
    -- (>>) :: m a -> m b -> m b
    -- a >> f = a >>= \_ -> f
    -- fail    

-- Monad é construtor de tipo (tipo parametrizado) que representa computação que 
-- retorna um valor do tipo recebido como parâmetro. A computação pode ser trivial, 
-- assim monad pode conter apenas informação sobre o tipo. Melhor, um tipo parametrizado
-- é uma instância de Monad se tem função que mapeia elemento do tipo recebido como parâmetro
-- em monad do elemento (elemento em computação que retorna o elemento), e função que mapeia 
-- funções em elementos para funções em monads dos elementos, e função que mapeia monad de monad 
-- de elemento em monad de elemento. Ou a primeira função mais uma como bind, que usa elemento retornado
-- por uma computação para gerar outra computação (compõe computações, usando info gerada pela
-- primeira e carregando estado da primeira para a segunda computação - estado resultante
-- da primeira é entrada para a segunda). Monad é um functor
--
-- Listas, com map, join e unit formam um monad... Daí notação usada
-- para compreensão de listas
--
-- Restrições nas funções: (unit a) <== k = k a     m <== unit = m

-- No caso específico de IO, o tipo parametrizado e as funções são as seguintes.
--
-- data IOO a = IOC ((String,String) -> (a,(String,String)))
-- instance Monad (IOO) where
--	return v = IOC (\io -> (v,io))
--	(IOC f) >>= g = IOC (\i -> let (v,i1) = f i
--                                    (IOC h) = g v
--                                  in (h i1))

t4 = do c <- getChar
        putChar c
        return (c == 'y') 

-- Nos exemplos abaixo, pChar e gChar seriam similares aos gC e pC do começo da aula.
-- A única diferença é que pChar retornaria ((),(i,o++[c])), ao invés de simplesmente (i,o++[c])
-- getChar = (IOC gChar), e putChar c = seria (IOC (pChar c))
{- 
do c <- getChar
   putChar c

= getChar >>= (\c -> putChar c)

= IO (\io -> let (v1,io1) = gChar io
                 (IO h1) = (\c -> putChar c) v1
              in (h1 io1))

= IO (\io -> let (v1,io1) = gChar io
                 (IO h1) = putChar v1
              in (h1 io1))

= IO (\io -> let (v1,io1) = gChar io
                 (IO h1) = (IO pChar v1) 
              in (h1 io1))

= IO (\io -> let (v1,io1) = gChar io
              in (pChar v1 io1))


do c <- getChar
   putChar c
   return (c == 'y')

= getChar >>= (\c -> (putChar c) >>= (\d -> return (c == 'y')))

= getChar >>= (\c -> (putChar c) >>= (\d -> IO (\io -> (c == ‘y’,io))))

= IO (\io1 -> let (v1,io2) = gChar io1
                  (IO h1) = (\c -> (putChar c) >>= (\d -> IO (\io -> (c == ‘y’,io)))) v1
              in (h1 io2))

= IO (\io1 -> let (v1,io2) = gChar io1
                  (IO h1) = (putChar v1) >>= (\d -> IO (\io -> (v1 == ‘y’,io))))
              in (h1 io2))

= IO (\io1 -> let (v1,io2) = gChar io1
              in ((\io3 -> let (v2,io4) = pChar v1 io3
                           in (v1 == ‘y’,io4)) io2))

= IO (\io1 -> let (v1,io2) = gChar io1
              in (let (v2,io4) = pChar v1 io2
                  in (v1 == ‘y’,io4)))

%%%
(putChar v1) >>= (\d -> IO (\io -> (v1 == ‘y’,io))))
	
= IO (\io3 -> let (v2,io4) = pChar v1 io3
                  (IO h2) = (\d -> IO (\io -> (v1 == ‘y’,io)))) v2
              in (h2 io4))

= IO (\io3 -> let (v2,io4) = pChar v1 io3
                  (IO h2) = (\d -> IO (\io -> (v1 == ‘y’,io)))) v2
              in (h2 io4))

= IO (\io3 -> let (v2,io4) = pChar v1 io3
                  (IO h2) = IO (\io -> (v1 == ‘y’,io)))
              in (h2 io4))

= IO (\io3 -> let (v2,io4) = pChar v1 io3
              in ((\io -> (v1 == ‘y’,io)) io4))

= IO (\io3 -> let (v2,io4) = pChar v1 io3
              in (v1 == ‘y’,io4))
%%%

-}

t44 = getChar >>= (\c -> (putChar c) >>= (\d -> return (c == 'y')))

readChars 0 = return ()
readChars n = do c <- getChar
                 putChar c
                 putChar '\n'
                 readChars (n-1)


sumChars l 0 = return ((sum . map (\c -> ord c)) l)
sumChars l n = do c <- getChar
                  putChar '\n'
                  sumChars (l ++ [c]) (n-1)


    

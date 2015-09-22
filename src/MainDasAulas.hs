module MainDasAulas where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao
import FuncoesDeAltaOrdemAplicacaoParcialMap
import InterpretacaoEAnaliseEstaticaDelinguagens
import ExameEscrito2014
import InferenciaDeTiposPolimorfismoCompreensaoDeListasEstado



-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn (show (nossohead [3,4,1]))
    putStrLn (show ex)
    putStrLn (show (eqsort [5,9,3,4,1]))

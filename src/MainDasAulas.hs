module MainDasAulas where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao
import FuncoesDeAltaOrdemAplicacaoParcialMap
--import InterpretacaoEAnaliseEstaticaDelinguagens
--import ExameEscrito2014
import InferenciaDeTiposPolimorfismoCompreensaoDeListasEstado
import ExistentialTypes



-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn (show (analisador p9))
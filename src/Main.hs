-- | Main entry point to the application.
module Main where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn (show (tamanhoBanco))
    
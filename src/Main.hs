-- | Main entry point to the application.
module Main where

import IntroducaoAProgramacaoFuncionalEHaskell
import DetalhesSobreListasERecursao

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn (show (nossohead [3,4,1]))
    putStrLn (show (tamanhoBanco))
    putStrLn (show (f2 0))
    putStrLn (show (count 3 [1,2,3,1,4,3,6,7,4,3]))
    putStrLn (show (count 3 [1,2,1,4,6,7,4]))
    putStrLn (show (member 3 [1,2,1,4,6,7,4]))
    putStrLn (show (member 3 [1,2,3,1,4,3,6,7,4,3]))
    putStrLn (show (union [3,4,2] [1,2,3,5]))
    putStrLn (show (intersection [3,4,2] [1,2,3,5]))
    putStrLn (show (diff [3,4,2] [1,2,3,5]))
    putStrLn (show (union [1,2,3] []))
    putStrLn (show (intersection [3,4,2] [1,22,33,5]))
    putStrLn (show (diff [3,4,2] [1,2,3,4,5]))
    
module Main where

    import Lib
    import System.IO
    import Parser (interpretLine, initS)

    --data BiblioState = BiblioState {bibStyle :: String}

    -- interpret :: IO ()
    interpret s = do
                    (s2, continue) <- interpretLine "> " s
                    if continue then interpret s2 else return ()

    -- main :: IO ()
    main = do
            putStrLn "Welcome to Biblio! Type 'quit' to exit"
            interpret initS


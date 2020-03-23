module Main where

    import System.IO (hFlush, stdout)
    import Control.Monad (forever)

    main :: IO ()
    main = do
        putStr "Input your name: "
        hFlush stdout
        name <- getLine
        forever $ do
            putStr ">>> "
            hFlush stdout
            line <- getLine
            putStrLn (name ++ " typed: " ++ line)

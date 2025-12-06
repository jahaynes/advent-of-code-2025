module Main ( main ) where

import Parser

main :: IO ()
main = do

    f <- readFile "./sample_6"

    putStrLn f
    putStrLn ""

    pure ()
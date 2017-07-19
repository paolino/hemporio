module Main where

import ParseCarico (readCaricoFromFile)
import System.Environment
import Algorithm
import Types

main :: IO ()
main = do
    a:_ <- getArgs
    c <- readCaricoFromFile a 
    mapM_ print $ transposeProdotti c



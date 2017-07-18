module Main where

import ParseCarico (readCaricoFromFile)
import System.Environment

main :: IO ()
main = do
    a:_ <- getArgs
    readCaricoFromFile a >>= print


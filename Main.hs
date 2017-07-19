
{-# language OverloadedStrings #-}
module Main where

import ParseCarico (readCaricoFromFile)
import System.Environment
import Algorithm
import Types
import Text.CSV
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Yaml (decodeFile)
import Data.Text.Template (template, render)
import Control.Monad
import Data.Semigroup 
m $%$ k = maybe (error $ "chiave " ++ show k) id $ M.lookup k m 
main :: IO ()
main = do
    csv:par:temp:out:_ <- getArgs
    c <- readCaricoFromFile csv
    r <- decodeFile par
    r <- case r of 
          Nothing -> error "sintassi in file par"
          Just r -> return r
    t <- template <$> T.readFile temp

    let ms  = map (renderDistribuzione2 r)
            . zip [1..]
            $ transposeProdotti c >>= expandMese 
    forM_ ms $ \m -> do
        let t' = render t (m $%$)
        LT.writeFile (out <> "/" <> T.unpack (m M.! "data") <> ".xhtml") $ t'




{-# language OverloadedStrings #-}
module Main where

import ParseInput (readInputFromFile)
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
    csv:par:tempS:tempR:out:_ <- getArgs
    c <- readInputFromFile csv
    r <- decodeFile par
    r <- case r of 
          Nothing -> error "sintassi in file par"
          Just r -> return r
    ts <- template <$> T.readFile tempS
    tr <- template <$> T.readFile tempR
    let ms  = map renderDistribuzione1
            . zip [1..]
            $ transposeProdotti c >>= snd . expandMese 

    forM_ ms $ \m -> do
        let m' = m <> r
            t' = render ts (m' $%$)
        LT.writeFile (out <> "/" <> T.unpack (m' M.! "data") <> ".xhtml") $ t'
    let mr  = renderOutputs (19,3)
            $ expandMese <$> transposeProdotti c

    forM_ mr $ \m -> do
        let m' = m <> r
            t' = render tr (maybe "--" id . flip M.lookup m')
        LT.writeFile 
            (out    <>  "/registro." 
                    <>  T.unpack (m' M.! "pagina")  
                    <>  ".xhtml") 
            $ t'


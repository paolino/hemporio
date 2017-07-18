{-# language DeriveFunctor #-}
{-# language ExplicitForAll #-}
module ParseCarico (readCaricoFromFile) where

import Control.Monad
import Text.CSV
import Control.Arrow
import Data.Time
import Parser
import Types
import Control.Applicative

parseADate :: forall b. ParseTime b => String -> Either [Char] b
parseADate x 
    =  maybe 
        (Left $ "date parsing error: " ++ x)
        Right
        $ parseTimeM True defaultTimeLocale "%Y/%m/%d" x

parseHeader :: Consume L Header
parseHeader 
    = Header 
    <$> field readE
    <*> field readE
    <*> field parseADate

parseHeaders :: Consume L [Header]
parseHeaders = do
    n <- subtract 3 <$> count -- count records in next line
    transposeN 3 -- transpose next 3 lines
    replicateM_ 3 line -- drop 3 lines
    replicateM n $ line >> parseHeader -- get headers

notNullText :: (Monad m, Alternative m) => [a] -> m [a]
notNullText x =  do
    guard (not $ null x) 
    return x

parseProdotto :: Consume L Prodotto
parseProdotto
    = Prodotto 
    <$> field notNullText
    <*> field notNullText
    <*> (field dontParse >> fields (readD 0))

parseCarico :: Consume L Carico
parseCarico = do
    hs <- parseHeaders
    line 
    textField "um"
    textField "prodotto"
    ps <- allLines parseProdotto
    line
    line >> textField "distribuzioni"
    ds <- allLines $ field parseADate
    return $ Carico hs ps ds

readCaricoFromFile :: FilePath -> IO Carico
readCaricoFromFile f = do
    ec <- parseCSVFromFile f
    either error return $ do
        c <- left show ec
        fst <$> runConsume parseCarico (L c [])



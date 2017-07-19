module Algorithm where

import Data.Time
import Types
import Control.Arrow ((***))
import Data.List (transpose, mapAccumL)

data Scarico = Scarico 
    {   prodotto :: String
    ,   quantità :: Int
    ,   misura :: String
    }

data Distribuzione = 
    Distribuzione 
         {  giorno :: Day
         ,  scarichi :: [Scarico]
         ,  utenti :: Int
         }

grouper :: [Header] -> [Day] -> [(Header,[Day])]
grouper hs ds = zip hs $ scanl f ds hs where
    f (_,ds)  (Header _ _ d) = let
        (ts,ds') = break (> d) ds
        in (
    
data Merce = Merce String String deriving Show
transposeProdotti :: Carico -> [((Header, [(Merce,Int)]))]
transposeProdotti (Carico hs ps ds) 
    = zip hs
    . (uncurry $ zipWith zip) 
    . (repeat *** transpose) 
    . unzip 
    . map (\(Prodotto um n is) -> (Merce um n,is)) 
    $ ps

run :: Carico -> [Distribuzione]
run = undefined
-- raggruppare le date di distribuzione tra i giorni di carico
--

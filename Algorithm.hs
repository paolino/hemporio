{-# language OverloadedStrings #-}
module Algorithm where

import Data.Time
import Types
import Control.Arrow ((***))
import Data.List 
import Data.Tuple
import Data.Text (Text, pack)
import Data.Map (fromList,Map)
import Data.Semigroup ((<>))
repart :: Int -> Int -> [Int]
repart n t = let
    (k,r) = t `divMod` n
    in (k + r) : replicate (n - 1) k

ripartizioneMensile :: Int -> (Merce,Int) -> [(Merce,Int)]
ripartizioneMensile n 
    = uncurry zip
    . (repeat *** repart n)


grouper :: [Header] -> [Day] -> [[Day]]
grouper hs ds 
    = (\(r, _:xs) -> xs ++ [r]) 
    $ mapAccumL f ds hs 
    where
    f ds  (Header _ _ d) = swap $ span (< d) ds
    
transposeProdotti :: Carico -> [Mese]
transposeProdotti (Carico hs ps ds) 
    = zipWith3 Mese hs (grouper hs ds)
    . (uncurry $ zipWith zip) 
    . (repeat *** transpose) 
    . unzip 
    . map (\(Prodotto um n is) -> (Merce um n,is)) 
    $ ps

expandMese 
    (Mese (Header ind ddt ingd) ds ms) =  
        zipWith3 Distribuzione 
            ds
            (transpose . map (ripartizioneMensile (length ds)) $ ms)
            (repeat ind)

tshow :: Show a => a -> Text
tshow = pack . show

renderDistribuzione2 
    :: Map Text Text 
    -> (Int, Distribuzione)
    -> Map Text Text
renderDistribuzione2 m (s,Distribuzione d ms i)
    = mappend m 
    . fromList 
    . mappend   [   ("numerosettimana", tshow s)
                ,   ("data" ,tshow d)
                ,   ("numeroutenti" , tshow i)
                ]   
    $ zip [1..] ms >>=  \(k,(Merce u n, q)) ->  
                    [   ("q" <> tshow k, tshow q)
                    ,   ("um" <> tshow k, pack u)
                    ,   ("pr" <> tshow k, pack n)
                    ]


renderDistribuzione1 (Distribuzione d ms i) = 
    [   ["giorno", show d]
    ,   ["utenti", show i]
    ] ++ map (\((Merce u n),q) -> [n,show q,u]) ms

-- raggruppare le date di distribuzione tra i giorni di carico
--

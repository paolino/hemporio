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
import Data.List.Split

repart :: Int -> Quantity -> [Quantity]
repart n (Quantity t) = let
    (k,r) = t `divMod` n
    in zipWith (+) (replicate r 1 ++ repeat 0) $ replicate n $ Quantity k

ripartizioneMensile :: Int -> (a,Quantity) -> [(a,(Quantity,Quantity))]
ripartizioneMensile n 
    = uncurry zip
    . (repeat *** conGiacenza n)

conGiacenza :: Int -> Quantity -> [(Quantity,Quantity)]
conGiacenza n q = (zip . (q:) <*> scanl (flip subtract) q)  . repart n $ q



grouper :: [Header] -> [Day] -> [[Day]]
grouper hs ds 
    = (\(r, _:xs) -> xs ++ [r]) 
    $ mapAccumL f ds hs 
    where
    f ds  (Header _ _ d) = swap $ span (< d) ds
    
transposeProdotti :: Input -> [Mese]
transposeProdotti (Input hs ps ds) 
    = zipWith3 Mese hs (grouper hs ds)
    . (uncurry $ zipWith zip) 
    . (repeat *** transpose) 
    . unzip 
    . map (\(Serie m is) -> (m,is)) 
    $ ps

type Output = ((Header,Giacenza), [Distribuzione]) 
expandMese :: Mese -> Output
expandMese 
    (Mese h@(Header ind ddt ingd) ds ms) =  let
        s:dqs = transpose . map (ripartizioneMensile (length ds)) $ ms
        in (,) (h,s) $ zipWith3 Distribuzione 
            ds
            dqs
            (repeat ind)

tshow :: Show a => a -> Text
tshow = pack . show

onlyPositive q x 
    | q > 0 = x
    | True = mempty

renderDistribuzione1 
    :: (Int, Distribuzione)
    -> Map Text Text
renderDistribuzione1 (s,Distribuzione d ms i)
    = fromList 
    . mappend   [   ("numerosettimana", tshow s)
                ,   ("data" ,tshow d)
                ,   ("numeroutenti" , tshow i)
                ]   
    $ zip [1..] ms >>=  \(k,(Prodotto u n, ((Quantity q),_))) ->  
                    [   ("q" <> tshow k, onlyPositive q $  tshow q )
                    ,   ("um" <> tshow k, onlyPositive q $ pack u)
                    ,   ("pr" <> tshow k, onlyPositive q $ pack n)
                    ]

data Mixed = Income (Header,Giacenza) | Outcome Int Distribuzione
fromOutput n (x,y) = (n + length y, Income x : zipWith Outcome [n..] y)

chunkMixed n (Income (h,g)) = map (\g -> Income (h,g)) $ chunksOf n g
chunkMixed n (Outcome l (Distribuzione d g i)) 
    = map (\g -> Outcome l (Distribuzione d g i)) $ chunksOf n g

renderSheet :: Int -> Int -> [Mixed] -> Map Text Text
renderSheet ir ic 
    = fromList 
    . (:) ("pagina" , tshow (ir,ic)) 
    . concatMap f 
    . zip [1..] 
    where
    f (n, Income (Header ind ddt d,  gs)) = 
        [   ("data_" <> tshow n, tshow d)
        ,   ("num_" <> tshow n, tshow ddt)
        ,   ("dest_" <> tshow n, tshow ind)
        ] ++ concatMap (f' n) (zip [1..] gs)
    f (n, Outcome l (Distribuzione d gs ind)) = 
        [   ("data_" <> tshow n, tshow d)
        ,   ("num_" <> tshow n, tshow l)
        ,   ("dest_" <> tshow n, tshow ind)
        ] ++ concatMap (f'' n) (zip [1..] gs)

    f' n (c, (Prodotto u k ,(Quantity q,Quantity g))) =
        [   ("umi_c" <> tshow c <> tshow n,onlyPositive q $  pack u)
        ,   ("car_c" <> tshow c <> tshow n,onlyPositive q $  tshow q)
        ,   ("sca_c" <> tshow c <> tshow n,"")
        ,   ("gia_c" <> tshow c <> tshow n,onlyPositive g $  tshow g)
        ,   ("prod_c" <> tshow c, pack k)
        ]

    f'' n (c, (Prodotto u k ,(Quantity q,Quantity g))) =
        [   ("umi_c" <> tshow c <> tshow n,"")
        ,   ("car_c" <> tshow c <> tshow n,"")
        ,   ("sca_c" <> tshow c <> tshow n,onlyPositive q $  tshow q)
        ,   ("gia_c" <> tshow c <> tshow n,onlyPositive g $  tshow g)
        ,   ("prod_c" <> tshow c, pack k)
        ]
renderOutputs :: (Int,Int) -> [Output] -> [Map Text Text]
renderOutputs (nr,nc) xs = do 
        (ir,rs) <- zip [1..] $ chunksOf nr (concat . snd . mapAccumL fromOutput 1 $ xs)
        fmap (uncurry $ renderSheet ir) . zip [1..] $ transpose $ map (chunkMixed nc) rs

        


 

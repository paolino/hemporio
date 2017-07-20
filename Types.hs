{-# language GeneralizedNewtypeDeriving #-}
module Types where

import Data.Time

newtype Quantity = Quantity Int deriving (Num,Eq,Show,Ord)

data Header = Header 
        {   indigenti :: Int
        ,   ddt :: Int
        ,   date :: Day
        } deriving Show

data Prodotto = Prodotto 
    {   nome :: String
    ,   um :: String
    } deriving Show

data Serie = Serie
    {   merce :: Prodotto
    ,   carico :: [Quantity]
    } deriving Show


data Input = Input
    {   headersI :: [Header]
    ,   prodottiI :: [Serie]
    ,   usciteI :: [Day]
    } deriving Show


type Scaffale a = [(Prodotto,a)]

type Giacenza = Scaffale  (Quantity,Quantity)

data Distribuzione = 
    Distribuzione 
         {  giornoD :: Day
         ,  scaffaleD :: Giacenza
         ,  utentiD :: Int
         } deriving Show


data Mese  = Mese
    {   headerM :: Header
    ,   usciteM :: [Day]
    ,   scaffaleM :: Scaffale Quantity
    } deriving Show


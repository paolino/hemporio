module Types where

import Data.Time

data Header = Header 
        {   indigenti :: Int
        ,   ddt :: Int
        ,   date :: Day
        } deriving Show


data Prodotto = Prodotto 
    {   um :: String
    ,   nome :: String
    ,   carico :: [Int]
    } deriving Show


data Carico = Carico 
    {   headers :: [Header]
    ,   prodotti :: [Prodotto]
    ,   cdistribuzioni :: [Day]
    } deriving Show

data Merce = Merce String String deriving Show

data Distribuzione = 
    Distribuzione 
         {  giorno :: Day
         ,  scarichi :: [(Merce,Int)]
         ,  utenti :: Int
         } deriving Show


data Mese  = Mese
    {   contesto :: Header
    ,   distribuzioni :: [Day]
    ,   ricariche :: [(Merce, Int)]
    } deriving Show


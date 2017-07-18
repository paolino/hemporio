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
    ,   distribuzioni :: [Day]
    } deriving Show




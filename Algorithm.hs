import Types

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



run :: Carico -> [Distribuzione]

-- raggruppare le date di distribuzione tra i giorni di carico
--

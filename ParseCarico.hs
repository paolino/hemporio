{-# language DeriveFunctor #-}

import Control.Monad
import Text.CSV
import Control.Arrow

newtype Consume c a = Consume {
	runConsume :: c -> Either String (a, c)
	} deriving Functor


instance Applicative (Consume c) where
	pure x = Consume $ \c -> Right (x,c)
	Consume f <*> Consume g = Consume $ \c ->  do
		(f', c')  <-  f c
		(x , c'') <- g c'
		return (f' x, c'')

instance Monad (Consume c) where
	Consume f >>= g = Consume $ \c -> do
		(x, c')  <-  f c
		let Consume g' = g x
		(x' , c'') <- g' c'
		return (x', c'')
		
	
data L = L CSV Record

line = Consume f where
	f (L [] r) =  Left "not enough lines"
	f (L (l:ls) r) = Right ((), L ls l)

field p = Consume f where
	f (L _ []) = Left "not enough fields"
	f (L cs (r:rs)) = p r >>= \x -> return (x, L cs rs)

fields p = Consume f where
	f l@(L ls []) = return ([],l)
	f (L cs (r:rs)) = p r >>= \x -> first (x:) <$> f (L cs rs)

readE x = case reads x of
	[(y,_)] -> Right y
	_ -> Left "failed parsing a field"

textField t = field $ \x ->
	if x == t then Right ()
		else Left "failed matching text field"

readField :: Read a => Consume L a
readField = field readE

newtype IndigentiPerMese = IndigentiPerMese [Int] deriving Show

header t = do
	replicateM_ 2 (field $ const $ Right ())
	textField t

parseIndigenti = do
	header "indigenti"
	IndigentiPerMese <$> fields readE

newtype DocumentoDiTrasporto = DocumentoDiTrasporto [Int] deriving Show

parseDocumentoDiTrasporto = do
	header "ddt"
	DocumentoDiTrasporto <$> fields readE


parsing = do
	line
	i <- parseIndigenti
	line
	d <- parseDocumentoDiTrasporto
	return (i,d)
main = do
	csv <- parseCSVFromFile "carico agea 2016.csv"
	case csv of
		Left x -> print x
		Right csv 
			-> do print 
			$ fmap fst 
			$ runConsume parsing (L csv [])

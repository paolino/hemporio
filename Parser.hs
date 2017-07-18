{-# language DeriveFunctor #-}
{-# language ExplicitForAll #-}

module Parser where

import Text.CSV
import Control.Arrow
import Data.List (transpose)

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
        
    
data L = L CSV Record deriving Show

line :: Consume L ()
line = Consume f where
    f (L [] _) =  Left "not enough lines"
    f (L (l:ls) _) = Right ((), L ls l)

allLines :: Consume L a -> Consume L [a]
allLines c@(Consume p) = do
        push >> line
        r <- Consume $ \l -> case p l of
            Left _ -> Right (Nothing,l)
            Right (x,_) -> Right (Just x, l)
        case r of
            Nothing -> return []
            Just x -> (x:) <$> (line >> allLines c)

field :: forall a. (Field -> Either [Char] a) -> Consume L a        
field p = Consume f where
    f (L _ []) = Left "not enough fields"
    f (L cs (r:rs)) = p r >>= \x -> return (x, L cs rs)

fields :: forall t. (Field -> Either String t) -> Consume L [t]
fields p = Consume f where
    f l@(L _ []) = return ([],l)
    f (L cs (r:rs)) = p r >>= \x -> first (x:) <$> f (L cs rs)

readE :: forall b. Read b => String -> Either [Char] b
readE x = case reads x of
    [(y,_)] -> Right y
    _ -> Left "failed parsing a field"

readD :: forall a b. Read b => b -> String -> Either a b
readD y = either (const $ Right y) Right . readE

textField :: Field -> Consume L ()
textField t = field $ \x ->
    if x == t then Right ()
        else Left "failed matching text field"

readField :: Read a => Consume L a
readField = field readE

transposeN :: Int -> Consume L ()
transposeN n = Consume f where
    f (L ls rs) 
        | length ls < n = Left "not enough lines to transpose"
        | True = let 
            (ts,ls') = splitAt n ls
            in Right ((), L (transpose ts ++ ls') rs)

dontParse :: forall b a. b -> Either a ()
dontParse = const $ Right ()

push :: Consume L ()
push = Consume $ \(L (l:ls) r) -> Right ((), L (l:l:ls) r)

count :: Consume L Int
count = do
    push 
    line
    length <$> fields dontParse



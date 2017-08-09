{-# language DataKinds, KindSignatures, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, StandaloneDeriving, AutoDeriveTypeable, ConstraintKinds, GADTs, ScopedTypeVariables #-}

import Resource
import qualified Data.Map as Map
import Data.Map  (Map)
import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import System.Random
import Control.Monad.Trans
import Control.Monad.Except
import Data.Typeable
import Control.Arrow ((&&&))
import Data.Function
import Data.Ord

data Key (a :: Resource) where
    Key :: Int -> Key a

deriving instance Ord (Key a) 
deriving instance Eq (Key a) 
deriving instance Show (Key a) 

type M a b = Map (Key a) b

data RIState = RIState 
    {   _empori         :: M Emporio        (String)
    ,   _amministratori :: M Amministratore (Key Emporio,String)
    ,   _utenti         :: M Utente         (Key Emporio, String)
    ,   _distribuzioni  :: M Distribuzione  (Key Emporio)
    ,   _super          :: Key Super
    } deriving Show


makeLenses 'RIState



data BackendError 
    = forall a. (Typeable a, Show a) => KeyNotPresent a
    | WrongSuperUser

instance Show BackendError where
    show (KeyNotPresent k) = "Unknown:" ++ show (k, typeOf k)
    show WrongSuperUser = "Not the superuser"

type Ctx m = (MonadError BackendError m, MonadIO m, MonadState RIState m)

ristate0 :: MonadIO m => m (Key Super, RIState)
ristate0 = do
    ks <- Key <$> liftIO randomIO
    return (ks,RIState mempty mempty mempty mempty ks)

newKey :: MonadIO m => m (Key a)
newKey = Key <$> liftIO (randomRIO (10000,99999))

checkSuper 
    :: (MonadError BackendError m, MonadState RIState m) 
    => Key Super
    -> m ()
checkSuper ks =  do
        s <- gets (view super) 
        when (s /= ks) $ throwError WrongSuperUser

check   :: (Typeable (Key a), Ctx m) 
        => Getting (M a b) RIState (M a b) 
        -> Key a 
        -> m b

check l k = do
    m <- gets $ view l
    case Map.lookup k m of
        Nothing -> throwError (KeyNotPresent k)
        Just v -> return v

instance Ctx m  => NewResource Key m Super Emporio where
    data Result m Emporio = NuovoEmporio (String -> m (Key Emporio))
    new (SessionSuper ks) _ = do
        checkSuper ks
        return . NuovoEmporio $ \name -> do
            ke <- newKey
            empori %= Map.insert ke name
            return ke

instance {-# OVERLAPPABLE#-} (Typeable a, Ctx m) => ResourceDependency Key m a where
    dependency ke = error $ "uncheckable " ++ show (typeOf ke)

instance Ctx m => NewResource Key m Super Amministratore where
    data Result m Amministratore = NuovoAmministratore (String -> m (Key Amministratore))
    new (SessionSuper ks) ke = do
        checkSuper ks
        _ <- check empori ke 
        return . NuovoAmministratore $ \name -> do
            ka <- newKey 
            amministratori %= Map.insert ka (ke,name)
            return ka


instance forall a. Typeable a => Show (Session Key a) where
    show s = show (typeOf (undefined :: Key a), sessionKey s)
instance Eq (Session Key a) where
    (==) = (==) `on` sessionKey
instance Ord (Session Key a) where
    compare = comparing sessionKey

type Interaction m (a :: Resource) (b :: Resource)  = Map (Session Key a) (Result m b)
{-


instance Ctx m => KeyDependency Key m Emporio where
    dependency ke =  error "who checked emporio ?"

-}    
{-
k, modules loaded: Main, Role.
*Main> (ks,r) <- ristate0 
*Main> a = creation (Request CreateEmporio "borgotaro" NuovoEmporio Null)
*Main> b = promote . Request "paolino" NuovoAmministratore
*Main>  runExceptT $ flip runStateT r (a ks >>= \ebt -> b ebt ks)
-}

main = do
    (ks,s0) <- ristate0 
    x <- runExceptT . flip runStateT s0 $ do
        NuovoEmporio f <- creation (Request  CreateEmporio  (SessionSuper ks) ArgumentNiente ) 
        borgotaro <- f "borgotaro"
        fornovo <- f "fornovo"
        NuovoAmministratore f <- 
            creation (Request  CreateAmministratore  (SessionSuper ks) (ArgumentEmporio borgotaro) ) 
        paolino <- f "paolino"
        giuseppe <- f "giuseppe"
        federico <- f "federico"

        NuovoAmministratore f <- 
            creation (Request  CreateAmministratore  (SessionSuper ks) (ArgumentEmporio fornovo) ) 
        mirco <- f "mirco"
        piero <- f "piero"
        return ()
    either print print x

{-# language GADTs, UndecidableInstances, TypeFamilies, DataKinds, KindSignatures, PolyKinds, TypeInType, FlexibleContexts, MultiParamTypeClasses, AllowAmbiguousTypes #-}


-- type definition and inter dependency
module Resource where

import Data.Kind (Type)

data Resource
    = Super -- main system admin
    | Amministratore -- shop admin
    | Commesso -- shop waiter
    | Servente -- shop servant
    | Utente -- client
    | Niente -- no role
    | Distribuzione -- shopping event
    | Emporio -- shop id
    | Acquisto -- shopped item
    | Prodotto -- item 

-- how to resolve a Resource to an resource index
type Resolve = Resource -> Type

-- possible session
data Session (k :: Resolve) (a :: Resource) where
    SessionNiente                               :: Session k Niente
    SessionSuper            :: k Super          -> Session k Super
    SessionAmministratore   :: k Amministratore -> Session k Amministratore
    SessionUtente           :: k Utente         -> Session k Utente
    SessionCommesso         :: k Commesso       -> Session k Commesso
    SessionServente         :: k Servente       -> Session k Servente

sessionKey :: Session k a -> Maybe (k a)
sessionKey SessionNiente = Nothing
sessionKey (SessionSuper k) = Just k
sessionKey (SessionAmministratore k) = Just k
sessionKey (SessionUtente k) = Just k
sessionKey (SessionCommesso k) = Just k
sessionKey (SessionServente k) = Just k

-- possible component
data Argument (k :: Resolve) (a :: Resource) where
    ArgumentNiente                              :: Argument k Niente
    ArgumentEmporio         :: k Emporio        -> Argument k Emporio
    ArgumentDistribuzione   :: k Distribuzione  -> Argument k Distribuzione
    ArgumentUtente          :: k Utente         -> Argument k Utente
    ArgumentProdotto        :: k Prodotto       -> Argument k Prodotto

-- dependencies of a Resource
type family Dependency (k :: Resource -> Type) (a :: Resource) :: Type where
    Dependency k Amministratore = k Emporio
    Dependency k Commesso = k Emporio
    Dependency k Super = ()
    Dependency k Emporio = ()
    Dependency k Distribuzione = (k Emporio)
    Dependency k Utente = k Emporio
    Dependency k Servente = (k Commesso, k Utente)
    Dependency k Acquisto = (k Servente, k Prodotto)
    Dependency k Prodotto = k Distribuzione


-- ftion API, fix session + argument + target type API
data Creation s a t where
    CreateEmporio            :: Creation Super            Niente            Emporio           
    CreateAmministratore     :: Creation Super            Emporio           Amministratore   
    CreateUtente             :: Creation Amministratore   Niente            Utente           
    CreateDistribuzione      :: Creation Amministratore   Niente            Distribuzione    
    CreateProdotto           :: Creation Amministratore   Distribuzione     Prodotto         
    CreateCommesso           :: Creation Niente           Distribuzione     Commesso         
    CreateServente           :: Creation Commesso         Utente            Servente         
    CreateAcquisto           :: Creation Servente         Prodotto          Acquisto         


-- request
data Request k s a t = Request (Creation s a t) (Session k s) (Argument k a)

-- backend dependency insertion
class NewResource k m s t where
    data Result m t
    new :: Session k s -- request author, to check authorization
        -> Dependency k t -- dependency
        -> m (Result m t) -- created target

-- backend dependency retrieval
class ResourceDependency  k m a where
    dependency :: k a -> m (Dependency k a)

--  a creation logic layer, takes care of getting the dependency types right
creation    ::  (   NewResource k m s t -- target creator
                ,   ResourceDependency k m s -- session dependency retrieval
                ,   ResourceDependency k m a -- argument dependency retrieval
                ,   Monad m -- context for operation
                )
            => Request k s a t
            -> m (Result m t)
             
creation (Request CreateEmporio s ArgumentNiente) 
    = new s ()
creation (Request CreateAmministratore s (ArgumentEmporio ke)) 
    = new s ke
creation (Request CreateCommesso s (ArgumentDistribuzione kd)) 
    = dependency kd >>= new s 
creation (Request CreateUtente s@(SessionAmministratore ks) ArgumentNiente ) 
    = dependency ks >>= new s 
creation (Request CreateServente s@(SessionCommesso kc) (ArgumentUtente ku) ) 
    = new s (kc,ku)
creation (Request CreateDistribuzione s@(SessionAmministratore ka) ArgumentNiente ) 
    = dependency ka >>= new s 
creation (Request CreateProdotto s (ArgumentDistribuzione kd)) 
    = new s kd
creation (Request CreateAcquisto s@(SessionServente ks) (ArgumentProdotto kp))
    = new s (ks,kp)


{-# language GADTs#-}
{-# language UndecidableInstances#-}
{-# language TypeFamilies#-}
{-# language DataKinds#-}
{-# language KindSignatures#-}
{-# language PolyKinds#-}
{-# language TypeInType#-}
{-# language FlexibleContexts#-}
{-# language MultiParamTypeClasses#-}
{-# language AllowAmbiguousTypes#-}
{-# language TemplateHaskell#-}
{-# language StandaloneDeriving#-}
{-# language Rank2Types#-}
{-# language DeriveFunctor#-}
{-# language ConstraintKinds,DuplicateRecordFields#-}
{-# language DisambiguateRecordFields #-}
{-# language ScopedTypeVariables #-}

-- Type safe machinary (mostly boilerplate) to handle session as paths through resources
-- 
-- Session is a backend/frontend agreement on what is the context user actions are to be intended
--
-- In this project, we want to allow resources to be indexed by tags in reality.
-- The user interaction context (session) is then defined (mostly)  by the (valid) sequence of tags
-- (scanned in reality or fed as links from client UI)
--
-- This is a brutal disruption with URI based API, mitigated (and superseeded ?) by the *augmenting reality* 
-- way of semantic disambiguation. 
--
-- Hypothesys: the effect of sequencing scans (logic path) of a 4D tagged reality is 
--              such a gain in the user experience, both in term of system knowledge (service semantic) 
--              and in term of interaction model (reality as virtuality indexer), 
--              that we can give up access to resources  as URI 
--              and start using them as component of a vectorial/relational interaction model.

module Session where

import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Identity
import Control.Lens hiding (Index)
import Control.Lens.TH
import Data.Dependent.Map
import Data.GADT.Compare.TH
import Data.GADT.Compare 
import Data.Ord
import Control.Applicative





{-
path is = path' is session where
    path' [] _ = []
    path' _ [] = error "no such path'"
    path' (i : is) ns = let
        Node x ms = ns !! i
        in x : path' is ms
data Trie = Node (Method Resource) [Trie] deriving Show
leaf x = Node x []
single x s = Node x [s]
session = 
    [   single (Protectable Super) $ single  (Write Emporio) $ leaf (Write Amministratore) 
    ,   Node (Protectable Amministratore)
        [   leaf (Write Prodotto)
        ,   leaf (Write Distribuzione)
        ,   leaf (Write Utente)
        ]
    , leaf (Read Utente)
    , single (Read Distribuzione) 
        $ single (Read Utente) 
        $ single (Write Acquisto) 
        $ leaf (Read Prodotto)
    ]
-}


type family Password a

data Protectable a = Protectable a | Locked (Password a) a | Unlocked a
data Method a 
    = Protactable a -- admit data level protection
    | Read a -- readeable
    | Write a  -- definable and modifyable
    deriving Show

-- used as values and types
data Resource
    = Super -- main system admin
    | Amministratore -- shop admin
    | Commesso -- shop waiter
    | Servente -- shop servant
    | Utente -- client
    | Distribuzione -- shopping event
    | Emporio -- shop id
    | Acquisto -- shopped item
    | Prodotto -- item  
    deriving (Read, Show)

type Path a = [(Method Resource,a)]  -- a zipped path through some resources

type family Name p (r :: Resource) -- names 
type family Static p (r :: Resource) -- static content fo(r :: Resource) (r :: Resource)esou(r :: Resource)ces
type family Time p -- time notion
type family Borsa p (k :: Index) (r :: Resource) -- shopping bag notion
type family Scaffale p (k :: Index)  (r :: Resource) -- shelf notion

-- how to resolve a Resource to an resource index
type Index = Resource -> Type

-- content notion for resources, external types are projected from p, k is projecting 
-- the kind Resource to a concrete Type (a.k.a. *) introducing the relational notion of 
-- primary index
data Content p (k :: Index) (a :: Resource) where
    ContentAmministratore   :: Name p Amministratore -- name of admin
                            -> k Emporio -- shop membership (index)
                            -> Content p k Amministratore
    ContentEmporio          :: Name p Emporio -- just its name 
                            -> Content p k Emporio
    ContentUtente           :: Name p Utente -- user name
                            -> k Emporio -- shop membership (index)
                            -> Content p k Utente
    ContentDistribuzione    :: Time p -- closed/open/closed  event time description
                            -> Scaffale p k Prodotto  -- products on sell
                            -> k Emporio -- membership
                            -> Content p k Distribuzione
    ContentCommesso         :: k Distribuzione -- event membership
                            -> Content p k Commesso 
    ContentAcquisto         :: k Commesso -- the engaged waiter
                            -> k Utente -- the engaged user
                            -> Borsa p k Prodotto  -- shopping bag
                            -> Content p k Acquisto
    ContentProdotto         :: Name p Prodotto -- product name
                            -> Static p Prodotto -- i.e. uri of the product image
                            -> k Emporio  -- probably wrong membership
                            -> Content p k Prodotto

-- protection state functor

data Protection c = Protected (Password c) c | Protectable c | UnProtectable c
        deriving Functor


-- higher kinded Identity to absorb 'p'
newtype IdentityF p a = IdentityF {runIdentityF :: a} deriving Functor

-- concrete in memory mapping from an index of a resource to its content  
-- still to be monad parametrized
--
-- mapping from index to protected resources
type MP p (k :: Index) (r :: Resource) 
    = Map (k r) (Protection p (Content p k r))

-- mapping from index to resources
type M p (k :: Index) (r :: Resource) 
    = Map (k r) (Protection (Content p k r))

data Database p k = Database 
    {   _utenti             :: M p k Utente -- can be password protected
    ,   _amministratori     :: M p k Amministratore -- can be password protected
    ,   _empori             :: M p k Emporio 
    ,   _prodotti           :: M p k Prodotto
    ,   _acquisti           :: M p k Acquisto
    ,   _distribuzioni      :: M p k Distribuzione
    ,   _super              :: Protection p (k Super) -- one only su
    }

makeLenses ''Database

-- declination of new resources insertion, driven by Content contructor in picking 'r'
insertDB :: Ord (k r) => k r -> Content p k r -> Database p k -> Database p k
insertDB k c@(ContentUtente{})          = over utenti           $ Map.insert k (Unprotected c)
insertDB k c@(ContentProdotto{})        = over prodotti         $ Map.insert k c
insertDB k c@(ContentAmministratore{})  = over amministratori   $ Map.insert k (Unprotected c)
insertDB k c@(ContentDistribuzione{})   = over distribuzioni    $ Map.insert k c
insertDB k c@(ContentAcquisto{})        = over acquisti         $ Map.insert k c
insertDB k c@(ContentEmporio{})         = over empori           $ Map.insert k c

-- fixing the fact that we will use one common underlying type  for indexing
data QR i (r :: Resource) where
    QRUtente            :: i -> QR i Utente 
    QRAmministratore    :: i -> QR i Amministratore
    QREmporio           :: i -> QR i Emporio
    QRDistribuzione     :: i -> QR i Distribuzione
    QRProdotto          :: i -> QR i Prodotto

-- extracting the real key
-- can't make this inline due to the GADTs  ? (investigate)
qrKey :: QR i r -> i
qrKey (QRUtente i) = i
qrKey (QRAmministratore i) = i
qrKey (QREmporio i) = i
qrKey (QRProdotto i) = i
qrKey (QRDistribuzione i) = i

-- promoting Qr i to be a Map key argument
deriving instance Eq i => Eq (QR i r)

instance Ord i => Ord (QR i r) where
    compare = comparing qrKey

-- CPS Map key membership
onMember :: (Ord k) => (k -> r) -> r -> k -> Map k a -> r
onMember f r k m = if k `Map.member` m then f k else r

-- CPS searching a key in the database, do something with the key if member checked
identifyResource :: Ord i =>  Database p (QR i) ->  i -> (forall r. QR i r -> Maybe a) -> Maybe a
identifyResource db i  f
    =   onMember f Nothing (QRUtente i) (db ^. utenti) 
    <|> onMember f Nothing (QRAmministratore i) (db ^. amministratori) 
    <|> onMember f Nothing (QREmporio i) (db ^. empori) 
    <|> onMember f Nothing (QRProdotto i) (db ^. prodotti) 
    <|> onMember f Nothing (QRDistribuzione i) (db ^. distribuzioni) 

--------------------------------------------------------------------------
--
-- context monoid to be zipped to r path
-- every resource added to the path introduces its key in the context
-- we need a polymorphic container, but as we have a finite set of types
--   DMAP applies. 
-- For the rank of Ctx we cannot derive the necessary instances for the keys
--
-- ----------------------------------------------------------

-- Keys of r DMAP each holding r typed index [CtxEmporio :=> value of typr (k Emporio),.....] 
data Ctx k r where
    CtxEmporio          :: Ctx k (k Emporio)
    CtxAmministratore   :: Ctx k (k Amministratore)
    CtxUtente           :: Ctx k (k Utente)
    CtxProdotto         :: Ctx k (k Prodotto)
    CtxDistribuzione    :: Ctx k (k Distribuzione)

-- specialise to Identity
type IdentityCtx k = DMap (Ctx k) Identity

-- insert an typed index
insertCtx :: Ctx k (k r) -> k r -> IdentityCtx k -> IdentityCtx k
insertCtx k v = insert k $ Identity v

-- get a typed index
getCtx :: Ctx k (k r)  -> IdentityCtx k  -> k r
getCtx k = runIdentity . flip (!) k

-- gimme TH. this is not deriveGEQ compatible due to the k :-/
instance GEq (Ctx k) where
    CtxEmporio `geq` CtxEmporio = Just Refl
    CtxUtente `geq` CtxUtente = Just Refl
    CtxProdotto `geq` CtxProdotto = Just Refl
    CtxAmministratore `geq` CtxAmministratore = Just Refl
    CtxDistribuzione `geq` CtxDistribuzione = Just Refl

    _ `geq` _ = Nothing

-- gimme TH
instance GCompare (Ctx k) where
    CtxEmporio `gcompare` CtxEmporio = GEQ
    CtxEmporio `gcompare` _ = GLT
    _ `gcompare` CtxEmporio = GGT
    CtxUtente `gcompare` CtxUtente = GEQ
    CtxUtente `gcompare` _ = GLT
    _ `gcompare` CtxUtente = GGT
    CtxProdotto `gcompare` CtxProdotto = GEQ
    CtxProdotto `gcompare` _ = GLT
    _ `gcompare` CtxProdotto = GGT
    CtxAmministratore `gcompare` CtxAmministratore = GEQ
    CtxAmministratore `gcompare` _ = GLT
    _ `gcompare` CtxAmministratore = GGT
    CtxDistribuzione `gcompare` CtxDistribuzione = GEQ
    -- CtxDistribuzione `gcompare` _ = GLT
    -- _ `gcompare` CtxDistribuzione = GGT
------------------------------------------------------------

-- our final session path augmented with the context of the indexed key
-- [    (p0 :: Read Distribuzione, fromList [CtxDistribuzione :=> QRDistribuzione (n :: i)])
-- ,    (   p1 :: Read Utente 
--          ,   fromList 
--                  [   CtxDistribuzione :=> QRDistribuzione (n :: i)]
--                  ,   CtxUtente :=> QRUtente (nu :: i)
--                  ] 
--      )
-- ]
-- the succ context is always by-one bigger the the previous, we already keep them like this 
-- for a session back jump forecasting
-- the client session should just query the last context 

type CtxPath i = [(Method Resource, IdentityCtx (QR i))]

------------------------------------------------------------------------------------------------
---
-- handling by-resource access, which is the core functionality
-- 
--
-- trie like context selection, where the search trie is ^session^ value in the comments
-- which we are not using :-/, the trie here is small 
--
-- the idea is to step 
--
--
------------------------------------------------------------------------------------


-- match a resource index in the database
oldResource :: forall p i . Ord i => Database p (QR i) -> CtxPath i -> i -> Maybe (CtxPath i)
-- unlogged access
oldResource db [] i = identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    -- amministratore password
    f q@(QRAmministratore u) = return [(Protectable Amministratore,insertCtx CtxAmministratore q $ mempty)]
    -- utente password
    f q@(QRUtente u) = return [(Protectable Utente,insertCtx CtxUtente q $ mempty)]
    -- super password
    f q@(QRUtente u) = return [(Protectable Super,mempty)]
    -- distribuzione read
    f q@(QRDistribuzione u) = return [(Read Distribuzione,insertCtx CtxDistribuzione q $ mempty)]
    f _ = Nothing

-- super -> emporio selection
oldResource db p@[(Protectable Super,w)] i = identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    f q@(QREmporio u) = return $ p ++ [(Write Emporio, insertCtx CtxEmporio q w)]
    f _ = Nothing

-- super + emporio + amministrazione
oldResource db 
            p@      [    (Protectable Super, _)
                    ,    (Write Emporio, w )
                    ] 
            i = identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    f q@(QRAmministratore u) = return $ p ++ [(Write Amministratore,insertCtx CtxAmministratore q w)]
    f _ = Nothing

-- amministratore + distribuzione
oldResource db p@[  (Protectable Amministratore, _)
                 ,  (Write Distribuzione,w)
                    ] i = (p ++) <$> identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    f q@(QRDistribuzione u) = return [(Write Distribuzione, insertCtx CtxDistribuzione q w)]
    f q@(QRUtente u)        = return [(Write Utente, insertCtx CtxUtente q w)]
    f q@(QRProdotto u)      = return [(Write Prodotto, insertCtx CtxProdotto q w)]
    f _ = Nothing

oldResource db p@   [  (Read Distribuzione,w)
                    ]
                i = (p ++) <$> identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    f q@(QRUtente u) =      return [(Read Utente, insertCtx CtxUtente q w)]
    f _ = Nothing

oldResource db p@   [  (Read Distribuzione,_)
                    ,  (Read Utente, w)
                    ]
                i = (p ++) <$> identifyResource db i f where
    f :: QR i r -> Maybe (CtxPath i)
    f q@(QRUtente u) =      return [(Read Utente, insertCtx CtxUtente q w)] -- swap utente
    f _ = Nothing
class Default u r where
    def :: u -> r

type Defaulting p i  =
                ( Default () (Name p Utente) 
                , Default () (Name p Prodotto)
                , Default i (Static p Prodotto)
                , Default () (Scaffale p (QR i) 'Prodotto)
                , Default () (Time p)
                ) 


newResource :: (Show i, Ord i, Defaulting p i)
            => CtxPath i 
            -> i 
            -> [(CtxPath i,  Database p (QR i) -> Database p (QR i))]
-- handle amministratore as resource creator
newResource p@[(Protectable Amministratore,ctx)] u 
    =   [   (   p ++ [(Write Utente, insertCtx CtxUtente (QRUtente u) ctx)]
            ,   insertDB  (QRUtente u) (ContentUtente (def ()) $ getCtx CtxEmporio ctx)
            )
        ,   (   p ++ [(Write Prodotto, insertCtx CtxProdotto (QRProdotto u) ctx)]
            ,   insertDB  (QRProdotto u) (ContentProdotto (def ()) (def u) $ getCtx CtxEmporio ctx)
            )
        ,   (   p ++ [(Write Distribuzione, insertCtx CtxDistribuzione (QRDistribuzione  u) ctx)]
            ,   insertDB  (QRDistribuzione u) 
                    (ContentDistribuzione (def ()) (def ()) $ getCtx CtxEmporio ctx)
            )
        ]  
-- handle super user as emporio resource creator
newResource p@[(Protectable Super,ctx)] u = undefined
-- handle super user as amministratore resource creator
newResource p@[(Protectable Super,_),(Write Emporio, ctx)] u = undefined


acceptResource 
        :: (Ord i, Show i, Defaulting p i)
        => Database p (QR i) 
        -> CtxPath i 
        -> i 
        -> [(CtxPath i, Database p (QR i) -> Database p (QR i))]
acceptResource db cp u
    = case oldResource db cp u of
        Just cp' -> [(cp',id)] -- accepted as step
        Nothing -> newResource cp u -- expansion chances

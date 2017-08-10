{-# language GADTs, UndecidableInstances, TypeFamilies, DataKinds, KindSignatures, PolyKinds, TypeInType, FlexibleContexts, MultiParamTypeClasses, AllowAmbiguousTypes, TemplateHaskell, StandaloneDeriving, Rank2Types, DeriveFunctor, ConstraintKinds  #-}

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

data Method a 
    = Protectable a -- admit data level protection
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

type family Password p
type ProtectionK = Type -> Type -> Type
data Protection p c = Protected (Password p) c | Unprotected c
        deriving Functor

newtype IdentityF p a = IdentityF {runIdentityF :: a} deriving Functor
-- concrete in memory mapping from an index of a resource to its content  
-- inside a functor 

type M p (f :: ProtectionK) (k :: Index) (r :: Resource) 
    = Map (k r) (f p (Content p k r))

data Database p k = Database 
    {   _utenti         :: M p Protection k Utente
    ,   _amministratori   :: M p Protection k Amministratore
    ,   _empori         :: M p IdentityF k Emporio
    ,   _prodotti       :: M p IdentityF k Prodotto
    ,   _acquisti       :: M p IdentityF k Acquisto
    ,   _distribuzioni  :: M p IdentityF k Distribuzione
    ,   _super          :: Protection p (k Super)
    }

makeLenses ''Database

--
insertDB :: Ord (k a) => k a -> Content p k a -> Database p k -> Database p k
insertDB k c@(ContentUtente{}) = over utenti $ Map.insert k (Unprotected c)
insertDB k c@(ContentProdotto{}) = over prodotti $ Map.insert k (IdentityF c)
    {- ............. -}
insertDB k c@(ContentDistribuzione{}) = over distribuzioni $ Map.insert k (IdentityF c)

data QR i (a :: Resource) where
    QRUtente            :: i -> QR i Utente 
    QRAmministratore    :: i -> QR i Amministratore
    QREmporio           :: i -> QR i Emporio
    QRDistribuzione     :: i -> QR i Distribuzione
    QRProdotto          :: i -> QR i Prodotto

qrKey :: QR i a -> i
qrKey (QRUtente i) = i
qrKey (QRAmministratore i) = i
qrKey (QREmporio i) = i
qrKey (QRProdotto i) = i
qrKey (QRDistribuzione i) = i

deriving instance Eq i => Eq (QR i a)

instance Ord i => Ord (QR i a) where
    compare = comparing qrKey

onMember :: (Ord k) => (k -> r) -> k -> Map k a -> r -> r
onMember f k m r = if k `Map.member` m then f k else r

identifyResource :: Ord i =>  Database p (QR i) ->  i -> (forall a. QR i a -> Maybe r) -> Maybe r

identifyResource db i f
    =   onMember f (QRUtente i) (db ^. utenti) Nothing 
    <|> onMember f (QRAmministratore i) (db ^. amministratori) Nothing
    {- .............. -}
--------------------------------------------------------------------------
--
-- context monoid to be zipped to a path
--
-- ----------------------------------------------------------

-- Keys of a DMAP each holding a typed index [CtxEmporio :=> value of typr (k Emporio),.....] 
data Ctx k a where
    CtxEmporio          :: Ctx k (k Emporio)
    CtxAmministratore   :: Ctx k (k Amministratore)
    CtxUtente           :: Ctx k (k Utente)
    CtxProdotto         :: Ctx k (k Prodotto)
    CtxDistribuzione    :: Ctx k (k Distribuzione)

insertCtx :: Ctx k (k r) -> k r -> DMap (Ctx k) Identity -> DMap (Ctx k) Identity
insertCtx k v = insert k $ Identity v
-- modifyCtx k f v = insertWith (\(Identity v1) (Identity v2) -> Identity (f v1 v2)) k (Identity v)

getCtx :: Ctx k (k r)  -> DMap (Ctx k) Identity  -> k r
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
    CtxDistribuzione `gcompare` _ = GLT
    _ `gcompare` CtxDistribuzione = GGT

type MContext k = DMap (Ctx k) Identity
type CtxPath i = [(Method Resource, MContext (QR i))]
------------------------------------------------------------------------------------------------
---
-- trie like context selection, where the search trie is ^session^ which we are not using :-/
--
------------------------------------------------------------------------------------
oldResource :: forall p i a . Ord i => Database p (QR i) -> CtxPath i -> i -> Maybe (CtxPath i)
-- unlogged access
oldResource db [] i = identifyResource db i f where
    f :: QR i a -> Maybe (CtxPath i)
    -- amministratore password
    f q@(QRAmministratore u) = return [(Protectable Amministratore,insertCtx CtxAmministratore q $ mempty)]
    -- utente password
    -- super password
    -- distribuzione read
    f _ = Nothing
-- super -> emporio selection
-- super + emporio + amministrazione

class Default u a where
    def :: u -> a

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

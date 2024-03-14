{-|
Module      : Gargantext.API.Ngrams.Types
Description : Ngrams List Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams.Types where

import Codec.Serialise (Serialise())
import Control.Category ((>>>))
import Control.Lens (makeLenses, makePrisms, Iso', iso, from, (.~), (.=), (?=), (#), to, folded, {-withIndex, ifolded,-} view, use, (^.), (^?), (%~), (.~), (%=), at, _Just, Each(..), itraverse_, both, forOf_, (?~), over)
import Data.Aeson hiding ((.=))
import Data.Aeson.TH (deriveJSON)
import Data.Csv (defaultEncodeOptions, encodeByNameWith, header, namedRecord, EncodeOptions(..), NamedRecord, Quoting(QuoteNone))
import Data.Csv qualified as Csv
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Data.Patch.Class (Replace(Keep), replace, Action(act), Group, Applicable(..), Composable(..), Transformable(..), PairPatch(..), Patched, ConflictResolution, ConflictResolutionReplace, MaybePatch(Mod), unMod, old, new)
import Data.Set qualified as Set
import Data.String (IsString(..))
import Data.Swagger ( NamedSchema(NamedSchema), declareSchemaRef, genericDeclareNamedSchema, SwaggerType(SwaggerObject), ToParamSchema, ToSchema(..), HasProperties(properties), HasRequired(required), HasType(type_) )
import Data.Text qualified as T
import Data.Validity ( Validity(..) )
import Database.PostgreSQL.Simple.FromField (FromField, fromField, fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField, toJSONField, toField)
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..), ListId, NodeId, TODO)
import Gargantext.Core.Types.Query (Limit, Offset, MaxSize, MinSize)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixUntagged, unPrefixSwagger, wellNamedSchema)
import Gargantext.Database.Admin.Types.Node (ContextId)
import Gargantext.Database.Prelude (fromField', HasConnectionPool, HasConfig, CmdM')
import Gargantext.Database.Schema.Ngrams qualified as TableNgrams
import Gargantext.Prelude hiding (IsString, hash, from, replace, to)
import Gargantext.Prelude.Crypto.Hash (IsHashable(..))
import Gargantext.Utils.Servant (CSV, ZIP)
import Gargantext.Utils.Zip (zipContentsPure)
import Servant ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece), Required, Strict, QueryParam', MimeRender(.. ))
import Servant.Job.Utils (jsonOptions)
import Test.QuickCheck (elements, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

type QueryParamR = QueryParam' '[Required, Strict]

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs   | Trash   | MoreFav | MoreTrash
               | Terms  | Sources | Authors | Institutes
               | Contacts
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)


instance Hashable TabType

instance FromHttpApiData TabType where
    parseUrlPiece "Docs"       = pure Docs
    parseUrlPiece "Trash"      = pure Trash
    parseUrlPiece "MoreFav"    = pure MoreFav
    parseUrlPiece "MoreTrash"  = pure MoreTrash

    parseUrlPiece "Terms"      = pure Terms
    parseUrlPiece "Sources"    = pure Sources
    parseUrlPiece "Institutes" = pure Institutes
    parseUrlPiece "Authors"    = pure Authors

    parseUrlPiece "Contacts"   = pure Contacts

    parseUrlPiece _            = Left "Unexpected value of TabType"
instance ToHttpApiData TabType where
    toUrlPiece = T.pack . show
instance ToParamSchema TabType
instance ToJSON        TabType
instance FromJSON      TabType
instance ToSchema      TabType
instance FromJSONKey TabType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey TabType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

newtype MSet a = MSet (Map a ())
  deriving (Eq, Ord, Show, Generic, Arbitrary, Semigroup, Monoid)

instance ToJSON a => ToJSON (MSet a) where
  toJSON     (MSet m) = toJSON     (Map.keys m)
  toEncoding (MSet m) = toEncoding (Map.keys m)

mSetFromSet :: Set a -> MSet a
mSetFromSet = MSet . Map.fromSet (const ())

mSetFromList :: Ord a => [a] -> MSet a
mSetFromList = MSet . Map.fromList . map (\x -> (x, ()))

-- mSetToSet :: Ord a => MSet a -> Set a
-- mSetToSet (MSet a) = Set.fromList ( Map.keys a)
mSetToSet :: Ord a => MSet a -> Set a
mSetToSet = Set.fromList . mSetToList

mSetToList :: MSet a -> [a]
mSetToList (MSet a) = Map.keys a

instance Foldable MSet where
  foldMap f (MSet m) = Map.foldMapWithKey (\k _ -> f k) m

instance (Ord a, FromJSON a) => FromJSON (MSet a) where
  parseJSON = fmap mSetFromList . parseJSON

instance (ToJSONKey a, ToSchema a) => ToSchema (MSet a) where
  -- TODO
  declareNamedSchema _ = wellNamedSchema "" (Proxy :: Proxy TODO)

------------------------------------------------------------------------
newtype NgramsTerm = NgramsTerm { unNgramsTerm :: Text }
  deriving (Ord, Eq, Show, Generic, ToJSONKey, ToJSON, FromJSON, Semigroup, Arbitrary, Serialise, ToSchema, Hashable, NFData, FromField, ToField)
instance IsHashable NgramsTerm where
  hash (NgramsTerm t) = hash t
instance Monoid NgramsTerm where
  mempty = NgramsTerm ""
instance FromJSONKey NgramsTerm where
  fromJSONKey = FromJSONKeyTextParser $ \t -> pure $ NgramsTerm $ T.strip t
instance IsString NgramsTerm where
  fromString s = NgramsTerm $ T.pack s


data RootParent = RootParent
  { _rp_root   :: NgramsTerm
  , _rp_parent :: NgramsTerm
  }
  deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_rp_") ''RootParent
makeLenses ''RootParent

data NgramsRepoElement = NgramsRepoElement
  { _nre_size        :: !Int
  , _nre_list        :: !ListType
  -- root is the top-most parent of ngrams
  , _nre_root        :: !(Maybe NgramsTerm)
  -- parent is the direct parent of this ngram
  , _nre_parent      :: !(Maybe NgramsTerm)
  , _nre_children    :: !(MSet NgramsTerm)
  }
  deriving (Ord, Eq, Show, Generic)
deriveJSON (unPrefix "_nre_") ''NgramsRepoElement
-- TODO
-- if ngrams & not size => size
-- drop occurrences
makeLenses ''NgramsRepoElement
instance ToSchema NgramsRepoElement where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nre_")
instance FromField NgramsRepoElement where
  fromField = fromJSONField
instance ToField NgramsRepoElement where
  toField = toJSONField

data NgramsElement =
     NgramsElement { _ne_ngrams      :: NgramsTerm
                   , _ne_size        :: Int
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Set ContextId
                   , _ne_root        :: Maybe NgramsTerm
                   , _ne_parent      :: Maybe NgramsTerm
                   , _ne_children    :: MSet  NgramsTerm
                   }
      deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_ne_") ''NgramsElement
makeLenses ''NgramsElement

mkNgramsElement :: NgramsTerm
                -> ListType
                -> Maybe RootParent
                -> MSet NgramsTerm
                -> NgramsElement
mkNgramsElement ngrams list' rp children =
  NgramsElement ngrams (size (unNgramsTerm ngrams)) list' mempty (_rp_root <$> rp) (_rp_parent <$> rp) children

newNgramsElement :: Maybe ListType -> NgramsTerm -> NgramsElement
newNgramsElement mayList ngrams =
  mkNgramsElement ngrams (fromMaybe MapTerm mayList) Nothing mempty

instance ToSchema NgramsElement where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ne_")

------------------------------------------------------------------------
newtype NgramsTable = NgramsTable [NgramsElement]
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

-- type NgramsList = NgramsTable

makePrisms ''NgramsTable

-- | Question: why these repetition of Type in this instance
-- may you document it please ?
instance Each NgramsTable NgramsTable NgramsElement NgramsElement where
  each = _NgramsTable . each

-- TODO discuss
-- | TODO Check N and Weight
{-
toNgramsElement :: [NgramsTableData] -> [NgramsElement]
toNgramsElement ns = map toNgramsElement' ns
    where
      toNgramsElement' (NgramsTableData _ p t _ lt w) = NgramsElement t lt' (round w) p' c'
        where
          p' = case p of
                 Nothing -> Nothing
                 Just x  -> lookup x mapParent
          c' = maybe mempty identity $ lookup t mapChildren
          lt' = maybe (panic "API.Ngrams: listypeId") identity lt

      mapParent :: Map Int Text
      mapParent   = Map.fromListWith (<>) $ map (\(NgramsTableData i _ t _ _ _) -> (i,t)) ns

      mapChildren :: Map Text (Set Text)
      mapChildren = Map.mapKeys (\i -> (maybe (panic "API.Ngrams.mapChildren: ParentId with no Terms: Impossible") identity $ lookup i mapParent))
                  $ Map.fromListWith (<>)
                  $ map (first fromJust)
                  $ filter (isJust . fst)
                  $ map (\(NgramsTableData _ p t _ _ _) -> (p, Set.singleton t)) ns
-}

mockTable :: NgramsTable
mockTable = NgramsTable
  [ mkNgramsElement "animal"  MapTerm        Nothing       (mSetFromList ["dog", "cat"])
  , mkNgramsElement "cat"     MapTerm       (rp "animal")  mempty
  , mkNgramsElement "cats"    StopTerm       Nothing       mempty
  , mkNgramsElement "dog"     MapTerm       (rp "animal")  (mSetFromList ["dogs"])
  , mkNgramsElement "dogs"    StopTerm      (rp "dog")     mempty
  , mkNgramsElement "fox"     MapTerm        Nothing       mempty
  , mkNgramsElement "object"  CandidateTerm  Nothing       mempty
  , mkNgramsElement "nothing" StopTerm       Nothing       mempty
  , mkNgramsElement "organic" MapTerm        Nothing       (mSetFromList ["flower"])
  , mkNgramsElement "flower"  MapTerm       (rp "organic") mempty
  , mkNgramsElement "moon"    CandidateTerm  Nothing       mempty
  , mkNgramsElement "sky"     StopTerm       Nothing       mempty
  ]
  where
    rp n = Just $ RootParent n n

instance ToSchema NgramsTable

------------------------------------------------------------------------
-- Searching in a Ngram Table

data OrderBy = TermAsc | TermDesc | ScoreAsc | ScoreDesc
             deriving (Generic, Enum, Bounded, Read, Show)

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "TermAsc"   = pure TermAsc
    parseUrlPiece "TermDesc"  = pure TermDesc
    parseUrlPiece "ScoreAsc"  = pure ScoreAsc
    parseUrlPiece "ScoreDesc" = pure ScoreDesc
    parseUrlPiece _           = Left "Unexpected value of OrderBy"

instance ToHttpApiData OrderBy where
  toUrlPiece = T.pack . show

instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy

-- | A query on a 'NgramsTable'.
data NgramsSearchQuery = NgramsSearchQuery
  { _nsq_limit       :: !Limit
  , _nsq_offset      :: !(Maybe Offset)
  , _nsq_listType    :: !(Maybe ListType)
  , _nsq_minSize     :: !(Maybe MinSize)
  , _nsq_maxSize     :: !(Maybe MaxSize)
  , _nsq_orderBy     :: !(Maybe OrderBy)
  , _nsq_searchQuery :: !(NgramsTerm -> Bool)
  }

------------------------------------------------------------------------
type NgramsTableMap = Map NgramsTerm NgramsRepoElement


-- CSV:
-- header: status\tlabel\tforms
-- item: map\taccountability\taccounting|&|accoutns|&|account
instance MimeRender CSV NgramsTableMap where
  -- mimeRender _ _val = encode ([] :: [(Text, Text)])
  mimeRender _ val = encodeByNameWith encOptions (header ["status", "label", "forms"]) $ fn <$> Map.toList val
    where
      encOptions = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t')
                                        , encQuoting = QuoteNone }
      fn :: (NgramsTerm, NgramsRepoElement) -> NamedRecord
      fn (NgramsTerm term, NgramsRepoElement { _nre_list, _nre_children }) =
        namedRecord [ "status" Csv..= toText _nre_list
                    , "label" Csv..= term
                    , "forms" Csv..= T.intercalate "|&|" (unNgramsTerm <$> mSetToList _nre_children)]
      toText :: ListType -> Text
      toText CandidateTerm = "candidate"
      toText MapTerm = "map"
      toText StopTerm = "stop"

------------------------------------------------------------------------
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data PatchSet a = PatchSet
  { _rem :: Set a
  , _add :: Set a
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''PatchSet
makePrisms ''PatchSet

instance ToJSON a => ToJSON (PatchSet a) where
  toJSON     = genericToJSON     $ unPrefix "_"
  toEncoding = genericToEncoding $ unPrefix "_"

instance (Ord a, FromJSON a) => FromJSON (PatchSet a) where
  parseJSON = genericParseJSON $ unPrefix "_"

{-
instance (Ord a, Arbitrary a) => Arbitrary (PatchSet a) where
  arbitrary = PatchSet <$> arbitrary <*> arbitrary

type instance Patched (PatchSet a) = Set a

type ConflictResolutionPatchSet a = SimpleConflictResolution' (Set a)
type instance ConflictResolution (PatchSet a) = ConflictResolutionPatchSet a

instance Ord a => Semigroup (PatchSet a) where
  p <> q = PatchSet { _rem = (q ^. rem) `Set.difference` (p ^. add) <> p ^. rem
                    , _add = (q ^. add) `Set.difference` (p ^. rem) <> p ^. add
                    } -- TODO Review

instance Ord a => Monoid (PatchSet a) where
  mempty = PatchSet mempty mempty

instance Ord a => Group (PatchSet a) where
  invert (PatchSet r a) = PatchSet a r

instance Ord a => Composable (PatchSet a) where
  composable _ _ = undefined

instance Ord a => Action (PatchSet a) (Set a) where
  act p source = (source `Set.difference` (p ^. rem)) <> p ^. add

instance Applicable (PatchSet a) (Set a) where
  applicable _ _ = mempty

instance Ord a => Validity (PatchSet a) where
  validate p = check (Set.disjoint (p ^. rem) (p ^. add)) "_rem and _add should be dijoint"

instance Ord a => Transformable (PatchSet a) where
  transformable = undefined

  conflicts _p _q = undefined

  transformWith conflict p q = undefined conflict p q

instance ToSchema a => ToSchema (PatchSet a)
-}

type AddRem = Replace (Maybe ())

remPatch, addPatch :: AddRem
remPatch = replace (Just ()) Nothing
addPatch = replace Nothing (Just ())

isRem :: Replace (Maybe ()) -> Bool
isRem = (== remPatch)

type PatchMap = PM.PatchMap

newtype PatchMSet a = PatchMSet (PatchMap a AddRem)
  deriving (Eq, Show, Generic, Validity, Semigroup, Monoid, Group,
            Transformable, Composable)

unPatchMSet :: PatchMSet a -> PatchMap a AddRem
unPatchMSet (PatchMSet a) = a

type ConflictResolutionPatchMSet a = a -> ConflictResolutionReplace (Maybe ())
type instance ConflictResolution (PatchMSet a) = ConflictResolutionPatchMSet a

-- TODO this breaks module abstraction
makePrisms ''PM.PatchMap

makePrisms ''PatchMSet

_PatchMSetIso :: Ord a => Iso' (PatchMSet a) (PatchSet a)
_PatchMSetIso = _PatchMSet . _PatchMap . iso f g . from _PatchSet
  where
    f :: Ord a => Map a (Replace (Maybe ())) -> (Set a, Set a)
    f = Map.partition isRem >>> both %~ Map.keysSet

    g :: Ord a => (Set a, Set a) -> Map a (Replace (Maybe ()))
    g (rems, adds) = Map.fromSet (const remPatch) rems
                  <> Map.fromSet (const addPatch) adds

instance Ord a => Action (PatchMSet a) (MSet a) where
  act (PatchMSet p) (MSet m) = MSet $ act p m

instance Ord a => Applicable (PatchMSet a) (MSet a) where
  applicable (PatchMSet p) (MSet m) = applicable p m

instance (Ord a, ToJSON a) => ToJSON (PatchMSet a) where
  toJSON     = toJSON . view _PatchMSetIso
  toEncoding = toEncoding . view _PatchMSetIso

instance (Ord a, FromJSON a) => FromJSON (PatchMSet a) where
  parseJSON = fmap (_PatchMSetIso #) . parseJSON

instance ToSchema a => ToSchema (PatchMSet a) where
  -- TODO
  declareNamedSchema _ = wellNamedSchema "" (Proxy :: Proxy TODO)

type instance Patched (PatchMSet a) = MSet a

instance ToSchema a => ToSchema (Replace a) where
  declareNamedSchema (_ :: Proxy (Replace a)) = do
    -- TODO Keep constructor is not supported here.
    aSchema <- declareSchemaRef (Proxy :: Proxy a)
    pure $ NamedSchema (Just "Replace") $ mempty
            & type_ ?~ SwaggerObject
            & properties .~
                InsOrdHashMap.fromList
                [ ("old", aSchema)
                , ("new", aSchema)
                ]
            & required .~ [ "old", "new" ]

data NgramsPatch
   = NgramsPatch { _patch_children :: !(PatchMSet NgramsTerm)
                 , _patch_list     :: !(Replace ListType)   -- TODO Map UserId ListType
                 }
   | NgramsReplace { _patch_old :: !(Maybe NgramsRepoElement)
                   , _patch_new :: !(Maybe NgramsRepoElement)
                   }
      deriving (Eq, Show, Generic)

-- The JSON encoding is untagged, this is OK since the field names are disjoints and thus the encoding is unambiguous.
-- TODO: the empty object should be accepted and treated as mempty.
deriveJSON (unPrefixUntagged "_") ''NgramsPatch
makeLenses ''NgramsPatch

-- TODO: This instance is simplified since we should either have the fields children and/or list
-- or the fields old and/or new.
instance ToSchema NgramsPatch where
  declareNamedSchema _ = do
    childrenSch <- declareSchemaRef (Proxy :: Proxy (PatchMSet NgramsTerm))
    listSch <- declareSchemaRef (Proxy :: Proxy (Replace ListType))
    nreSch <- declareSchemaRef (Proxy :: Proxy NgramsRepoElement)
    pure $ NamedSchema (Just "NgramsPatch") $ mempty
            & type_ ?~ SwaggerObject
            & properties .~
                InsOrdHashMap.fromList
                [ ("children", childrenSch)
                , ("list",     listSch)
                , ("old",      nreSch)
                , ("new",      nreSch)
                ]
instance FromField NgramsPatch where
  fromField = fromJSONField
instance ToField NgramsPatch where
  toField = toJSONField


type NgramsPatchIso =
  MaybePatch NgramsRepoElement (PairPatch (PatchMSet NgramsTerm) (Replace ListType))

_NgramsPatch :: Iso' NgramsPatch NgramsPatchIso
_NgramsPatch = iso unwrap wrap
  where
    unwrap (NgramsPatch c l) = Mod $ PairPatch (c, l)
    unwrap (NgramsReplace o n) = replace o n
    wrap x =
      case unMod x of
        Just (PairPatch (c, l)) -> NgramsPatch c l
        Nothing -> NgramsReplace (x ^? old . _Just) (x ^? new . _Just)

instance Semigroup NgramsPatch where
  p <> q = _NgramsPatch # (p ^. _NgramsPatch <> q ^. _NgramsPatch)

instance Monoid NgramsPatch where
  mempty = _NgramsPatch # mempty

instance Validity NgramsPatch where
  validate p = p ^. _NgramsPatch . to validate

instance Transformable NgramsPatch where
  transformable p q = transformable (p ^. _NgramsPatch) (q ^. _NgramsPatch)

  conflicts p q = conflicts (p ^. _NgramsPatch) (q ^. _NgramsPatch)

  transformWith conflict p q = (_NgramsPatch # p', _NgramsPatch # q')
    where
      (p', q') = transformWith conflict (p ^. _NgramsPatch) (q ^. _NgramsPatch)

type ConflictResolutionNgramsPatch =
  ( ConflictResolutionReplace (Maybe NgramsRepoElement)
  , ( ConflictResolutionPatchMSet NgramsTerm
    , ConflictResolutionReplace ListType
    )
  , (Bool, Bool)
  )
type instance ConflictResolution NgramsPatch =
  ConflictResolutionNgramsPatch

type PatchedNgramsPatch = Maybe NgramsRepoElement
type instance Patched NgramsPatch = PatchedNgramsPatch

instance Applicable (PairPatch (PatchMSet NgramsTerm) (Replace ListType)) NgramsRepoElement where
  applicable (PairPatch (c, l)) n = applicable c (n ^. nre_children) <> applicable l (n ^. nre_list)

instance Action (PairPatch (PatchMSet NgramsTerm) (Replace ListType)) NgramsRepoElement where
  act (PairPatch (c, l)) = (nre_children %~ act c)
                         . (nre_list     %~ act l)

instance Applicable NgramsPatch (Maybe NgramsRepoElement) where
  applicable p = applicable (p ^. _NgramsPatch)
instance Action NgramsPatch (Maybe NgramsRepoElement) where
  act p = act (p ^. _NgramsPatch)

instance Action (Replace ListType) NgramsRepoElement where
  -- Rely on the already-defined instance 'Action (Replace a) a'.
  act replaceP = over nre_list (act replaceP)

newtype NgramsTablePatch = NgramsTablePatch (PatchMap NgramsTerm NgramsPatch)
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Semigroup, Monoid, Validity, Transformable)

mkNgramsTablePatch :: Map NgramsTerm NgramsPatch -> NgramsTablePatch
mkNgramsTablePatch = NgramsTablePatch . PM.fromMap

instance FromField NgramsTablePatch
  where
    fromField = fromJSONField
    --fromField = fromField'
instance ToField NgramsTablePatch
  where
    toField = toJSONField

instance FromField (PatchMap TableNgrams.NgramsType (PatchMap NodeId NgramsTablePatch))
  where
    fromField = fromField'

type instance ConflictResolution NgramsTablePatch =
  NgramsTerm -> ConflictResolutionNgramsPatch


type PatchedNgramsTablePatch = Map NgramsTerm PatchedNgramsPatch
  -- ~ Patched (PatchMap NgramsTerm NgramsPatch)
type instance Patched NgramsTablePatch = PatchedNgramsTablePatch

makePrisms ''NgramsTablePatch
instance ToSchema  (PatchMap NgramsTerm NgramsPatch)
instance ToSchema  NgramsTablePatch

instance Applicable NgramsTablePatch (Maybe NgramsTableMap) where
  applicable p = applicable (p ^. _NgramsTablePatch)


ngramsElementToRepo :: NgramsElement -> NgramsRepoElement
ngramsElementToRepo
  (NgramsElement { _ne_size     = s
                 , _ne_list     = l
                 , _ne_root     = r
                 , _ne_parent   = p
                 , _ne_children = c
                 }) =
  NgramsRepoElement
    { _nre_size     = s
    , _nre_list     = l
    , _nre_parent   = p
    , _nre_root     = r
    , _nre_children = c
    }

ngramsElementFromRepo :: NgramsTerm -> NgramsRepoElement -> NgramsElement
ngramsElementFromRepo
  ngrams
  (NgramsRepoElement
      { _nre_size     = s
      , _nre_list     = l
      , _nre_parent   = p
      , _nre_root     = r
      , _nre_children = c
      }) =
  NgramsElement { _ne_size        = s
                , _ne_list        = l
                , _ne_root        = r
                , _ne_parent      = p
                , _ne_children    = c
                , _ne_ngrams      = ngrams
                , _ne_occurrences = mempty -- panic $ "API.Ngrams.Types._ne_occurrences"
                {-
                -- Here we could use 0 if we want to avoid any `panic`.
                -- It will not happen using getTableNgrams if
                -- getOccByNgramsOnly provides a count of occurrences for
                -- all the ngrams given.
                -}
                }

reRootChildren :: NgramsTerm -> NgramsTerm -> State NgramsTableMap ()
reRootChildren root ngram = do
  nre <- use $ at ngram
  forOf_ (_Just . nre_children . folded) nre $ \child -> do
    at child . _Just . nre_root ?= root
    reRootChildren root child

reParent :: Maybe RootParent -> NgramsTerm -> State NgramsTableMap ()
reParent rp child = do
  at child . _Just %= ( (nre_parent .~ (_rp_parent <$> rp))
                      . (nre_root   .~ (_rp_root   <$> rp))
                      )
  reRootChildren (fromMaybe child (rp ^? _Just . rp_root)) child

reParentAddRem :: RootParent -> NgramsTerm -> AddRem -> State NgramsTableMap ()
reParentAddRem rp child p =
  reParent (if isRem p then Nothing else Just rp) child

-- | For each (k,v) of the 'PatchMap', transform the input 'NgramsTableMap'.
reParentNgramsPatch :: NgramsTerm
                    -- ^ The 'k' which is the target of the transformation.
                    -> NgramsPatch
                    -- ^ The patch to be applied to 'k'.
                    -> State NgramsTableMap ()
reParentNgramsPatch parent ngramsPatch = do
  root_of_parent <- use (at parent . _Just . nre_root)
  children       <- use (at parent . _Just . nre_children)
  let
    root     = fromMaybe parent root_of_parent
    rp       = RootParent { _rp_root = root, _rp_parent = parent }

  -- Apply whichever transformation has being applied to the parent also to its children.
  -- This is /not/ the same as applying 'patch_children' as in the 'itraverse_' below,
  -- because that modifies the tree by adding or removing children, and it will be triggered
  -- only if we have a non-empty set for 'patch_children'.
  forM_ children $ \childTerm -> do
    child <- use (at childTerm)
    case child of
      Nothing -> pure ()
      Just c
        -- We don't need to check if the patch is applicable, because we would be calling
        -- 'Applicable (Replace ListType) NgramsRepoElement' which is /always/ satisfied
        -- being 'ListType' a field of 'NgramsRepoElement'.
        | NgramsPatch{_patch_list} <- ngramsPatch
        -> at childTerm . _Just .= act _patch_list c
        | otherwise
        -> pure () -- ignore the patch and carry on.

  -- Finally, add or remove children according to the patch.
  itraverse_ (reParentAddRem rp) (ngramsPatch ^. patch_children . _PatchMSet . _PatchMap)
  -- TODO FoldableWithIndex/TraversableWithIndex for PatchMap

reParentNgramsTablePatch :: NgramsTablePatch -> State NgramsTableMap ()
reParentNgramsTablePatch p = itraverse_ reParentNgramsPatch (p ^. _NgramsTablePatch. _PatchMap)
  -- TODO FoldableWithIndex/TraversableWithIndex for PatchMap

------------------------------------------------------------------------

instance Action NgramsTablePatch (Maybe NgramsTableMap) where
  act p =
    fmap (execState (reParentNgramsTablePatch p)) .
    act (p ^. _NgramsTablePatch)

-- Should it be less than an Lens' to preserve PatchMap's abstraction.
-- ntp_ngrams_patches :: Lens' NgramsTablePatch (Map NgramsTerm NgramsPatch)
-- ntp_ngrams_patches = _NgramsTablePatch .  undefined

------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }
  deriving (Generic, Show, Eq)
deriveJSON (unPrefix "_v_") ''Versioned
makeLenses ''Versioned
instance (Typeable a, ToSchema a) => ToSchema (Versioned a) where
  declareNamedSchema = wellNamedSchema "_v_"
------------------------------------------------------------------------
type Count = Int

data VersionedWithCount a = VersionedWithCount
  { _vc_version :: Version
  , _vc_count   :: Count
  , _vc_data    :: a
  }
  deriving (Generic, Show, Eq)
deriveJSON (unPrefix "_vc_") ''VersionedWithCount
makeLenses ''VersionedWithCount
instance (Typeable a, ToSchema a) => ToSchema (VersionedWithCount a) where
  declareNamedSchema = wellNamedSchema "_vc_"

toVersionedWithCount :: Count -> Versioned a -> VersionedWithCount a
toVersionedWithCount count (Versioned version data_) = VersionedWithCount version count data_
------------------------------------------------------------------------

-- | TOREMOVE
data Repo s p = Repo
  { _r_version :: !Version
  , _r_state   :: !s
  , _r_history :: ![p]
    -- first patch in the list is the most recent
  }
  deriving (Generic, Show)

----------------------------------------------------------------------

instance (FromJSON s, FromJSON p) => FromJSON (Repo s p) where
  parseJSON = genericParseJSON $ unPrefix "_r_"

instance (ToJSON s, ToJSON p) => ToJSON (Repo s p) where
  toJSON     = genericToJSON     $ unPrefix "_r_"
  toEncoding = genericToEncoding $ unPrefix "_r_"

makeLenses ''Repo

initRepo :: Monoid s => Repo s p
initRepo = Repo 1 mempty []



--------------------

type RepoCmdM   env err m =
  ( CmdM'             env err m
  , HasConnectionPool env
  , HasConfig         env
  )


------------------------------------------------------------------------


-- Instances
instance FromHttpApiData (Map TableNgrams.NgramsType (Versioned NgramsTableMap))
  where
    parseUrlPiece x = maybeToEither x (decode $ cs x)

instance ToHttpApiData (Map TableNgrams.NgramsType (Versioned NgramsTableMap)) where
  toUrlPiece m = cs (encode m)

ngramsTypeFromTabType :: TabType -> TableNgrams.NgramsType
ngramsTypeFromTabType tabType =
  let here = "Garg.API.Ngrams: " :: Text in
    case tabType of
      Sources    -> TableNgrams.Sources
      Authors    -> TableNgrams.Authors
      Institutes -> TableNgrams.Institutes
      Terms      -> TableNgrams.NgramsTerms
      _          -> panicTrace $ here <> "No Ngrams for this tab"
      -- TODO: This `panic` would disapear with custom NgramsType.

----
-- Async task

data UpdateTableNgramsCharts = UpdateTableNgramsCharts
  { _utn_tab_type :: !TabType
  , _utn_list_id  :: !ListId
  } deriving (Eq, Show, Generic)

makeLenses ''UpdateTableNgramsCharts
instance FromJSON UpdateTableNgramsCharts where
  parseJSON = genericParseJSON $ jsonOptions "_utn_"

instance ToJSON UpdateTableNgramsCharts where
  toJSON = genericToJSON $ jsonOptions "_utn_"

instance ToSchema UpdateTableNgramsCharts where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_utn_")

------------------------------------------------------------------------
type NgramsList = (Map TableNgrams.NgramsType (Versioned NgramsTableMap))


-- | Same as NgramsList, but wraps node_id so that the inner .json file can have proper name
data NgramsListZIP =
  NgramsListZIP { _nlz_nl      :: NgramsList
                , _nlz_list_id :: ListId } deriving (Generic)
instance ToSchema NgramsListZIP where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nlz_")
nlzFileName :: NgramsListZIP -> Text
nlzFileName (NgramsListZIP { .. }) = "GarganText_NgramsList-" <> show _nlz_list_id <> ".json"
    
instance MimeRender ZIP NgramsListZIP where
  mimeRender _ nlz@(NgramsListZIP { .. }) =
    zipContentsPure (T.unpack $ nlzFileName nlz) (encode _nlz_nl)



--
-- Serialise instances
--

instance Serialise ListType
instance Serialise NgramsRepoElement
instance Serialise NgramsTablePatch
instance Serialise (PatchMap NgramsTerm NgramsPatch)
instance Serialise (MSet NgramsTerm)
instance Serialise AddRem
instance Serialise NgramsPatch
instance Serialise (Replace ListType)
instance (Serialise a, Ord a) => Serialise (PatchMap a AddRem)
instance (Serialise a, Ord a) => Serialise (PatchMSet a)
instance (Serialise s, Serialise p) => Serialise (Repo s p)

--
-- Arbitrary instances
--
instance Arbitrary     TabType where
  arbitrary = elements [minBound .. maxBound]
instance Arbitrary NgramsElement where
  arbitrary = elements [newNgramsElement Nothing "sport"]
instance Arbitrary NgramsTable where
  arbitrary = pure mockTable
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]
instance (Ord a, Arbitrary a) => Arbitrary (PatchMSet a) where
  arbitrary = (PatchMSet . PM.fromMap) <$> arbitrary
instance (Eq a, Arbitrary a) => Arbitrary (Replace a) where
  arbitrary = uncurry replace <$> arbitrary
    -- If they happen to be equal then the patch is Keep.
instance Arbitrary NgramsPatch where
  arbitrary = frequency [ (9, NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary))
                        , (1, NgramsReplace <$> arbitrary <*> arbitrary)
                        ]
instance Arbitrary NgramsTablePatch where
  arbitrary = NgramsTablePatch <$> PM.fromMap <$> arbitrary
instance Arbitrary a => Arbitrary (Versioned a) where
  arbitrary = Versioned 1 <$> arbitrary -- TODO 1 is constant so far
instance Arbitrary a => Arbitrary (VersionedWithCount a) where
  arbitrary = VersionedWithCount 1 1 <$> arbitrary -- TODO 1 is constant so far
instance Arbitrary NgramsRepoElement where
  arbitrary = elements $ map ngramsElementToRepo ns
    where
      NgramsTable ns = mockTable


toNgramsPatch :: [NgramsTerm] -> NgramsPatch
toNgramsPatch children = NgramsPatch children' Keep
  where
    children' :: PatchMSet NgramsTerm
    children' = PatchMSet
              $ fst
              $ PM.fromList
              $ List.zip children (List.cycle [addPatch])

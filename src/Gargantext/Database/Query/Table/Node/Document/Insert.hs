{-|
Module      : Gargantext.Database.Node.Document.Insert
Description : Importing context of texts (documents)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

* Purpose of this module

Enabling "common goods" of text data and respecting privacy.

Gargantext shares as "common good" the links between context of texts
and terms / words / ngrams.

Basically a context of text can be defined as a document (see 'Gargantext.Core.Text').

Issue to tackle in that module: each global document of Gargantext has
to be unique, then shared, but how to respect privacy if needed ?


* Methodology to get uniqueness and privacy by design

As a consequence, when importing/inserting a new document in Gargantext,
a policy for the uniqueness of the inserted docuemnts has to be defined.

That is the purpose of this module which defines its main concepts.

Unique identifier in database is of a 3-tuple of 3 policies that
together define uniqueness:

- Design policy: type of node is needed as TypenameId, that is a
Document or Individual or something else;

- Privacy policy: with ParentId, parent becomes unique, then it enables
users to get their own copy without sharing it with all the users of the
database (in others words parent_id is necessary to preserve privacy for
instance).

- Hash policy: this UniqId is a sha256 uniq id which is the result of
the concatenation of the parameters defined by @shaParameters@.

> -- * Example
> insertTest :: FromRow r => CorpusId -> [Node HyperdataDocument] -> IO [r]
> insertTest :: IO [ReturnId]
> insertTest = runCmdDev $ insertDocuments 1 452162 hyperdataDocuments

-}
------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------
module Gargantext.Database.Query.Table.Node.Document.Insert
  where

import Data.Aeson (toJSON, ToJSON)
import Data.Text qualified as DT (pack, concat, take, filter, toLower)
import Data.Time.Segment (jour)
import Database.PostgreSQL.Simple (FromRow, Query, Only(..))
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Database.PostgreSQL.Simple.ToField (toField, Action{-, ToField-})
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Core (HasDBid(toDBid))
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (runPGSQuery, DBCmd{-, formatPGSQuery-})
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude hiding (hash, toLower)
import Gargantext.Prelude.Crypto.Hash (hash)

{-| To Print result query
import Data.ByteString.Internal (ByteString)
import Database.PostgreSQL.Simple (formatQuery)
-}

---------------------------------------------------------------------------
-- * Main Insert functions

-- | Insert Document main function
-- UserId : user who is inserting the documents
-- ParentId : folder ID which is parent of the inserted documents
-- Administrator of the database has to create a uniq index as following SQL command:
-- `create unique index on contexts table (typename, parent_id, (hyperdata ->> 'uniqId'));`
insertDb :: (InsertDb a, HasDBid NodeType) => UserId -> Maybe ParentId -> [a] -> DBCmd err [ReturnId]
insertDb u p = runPGSQuery queryInsert . Only . Values fields . map (insertDb' u p)
      where
        fields    = map (QualifiedIdentifier Nothing) inputSqlTypes

class InsertDb a
  where
    insertDb' :: HasDBid NodeType => UserId -> Maybe ParentId -> a -> [Action]


instance InsertDb HyperdataDocument
  where
    insertDb' u p h = [ toField ("" :: Text)
                      , toField $ toDBid NodeDocument
                      , toField u
                      , toField p
                      , toField $ maybe "No Title" (DT.take 255)  (_hd_title h)
                      , toField $ _hd_publication_date h -- TODO USE UTCTime
                      -- , (toField . toJSON) (addUniqId h)
                      ]

instance InsertDb HyperdataContact
  where
    insertDb' u p _h = [ toField ("" :: Text)
                      , toField $ toDBid NodeContact
                      , toField u
                      , toField p
                      , toField $ maybe "Contact" (DT.take 255) (Just "Name") -- (_hc_name h)
                      , toField $ jour 0 1 1 -- TODO put default date
                      -- , (toField . toJSON) (addUniqId h)
                      ]

instance ToJSON a => InsertDb (Node a)
  where
    insertDb' _u _p (Node _nid hashId t u p n d h) = [ toField hashId
                                                     , toField t
                                                     , toField u
                                                     , toField p
                                                     , toField n
                                                     , toField d
                                                     , (toField . toJSON) h
                                                     ]

-- | Debug SQL function
--
-- to print rendered query (Debug purpose) use @formatQuery@ function.
{-
insertDocuments_Debug :: (Hyperdata a, ToJSON a, ToRow a, InsertDb [a])
                      => UserId -> ParentId -> [a] -> Cmd err ByteString
insertDocuments_Debug uId pId hs = formatPGSQuery queryInsert (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = insertDb' uId pId hs
-}

-- | Input Tables: types of the tables
inputSqlTypes :: [Text]
inputSqlTypes = map DT.pack ["text", "int4","int4","int4","text","date","jsonb"]

-- | SQL query to insert documents inside the database
queryInsert :: Query
queryInsert = [sql|
    WITH input_rows(hash_id,typename,user_id,parent_id,name,date,hyperdata) AS (?)
    , ins AS (
       INSERT INTO contexts (hash_id, typename,user_id,parent_id,name,date,hyperdata)
       SELECT * FROM input_rows
       ON CONFLICT (hash_id) DO NOTHING -- on unique index -- this does not return the ids
       RETURNING id,hash_id
       )

    SELECT true AS source                     -- true for 'newly inserted'
         , id
         , hash_id
    FROM   ins
    UNION  ALL
    SELECT false AS source                    -- false for 'not inserted'
         , n.id
         , hash_id
    FROM   input_rows
    JOIN   contexts n USING (hash_id);         -- columns of unique index
           |]

------------------------------------------------------------------------
-- * Main Types used
-- ** Return Types

-- | When documents are inserted
-- ReturnType after insertion
data ReturnId = ReturnId { reInserted :: Bool -- if the document is inserted (True: is new, False: is not new)
                         , reId       :: NodeId  -- always return the id of the document (even new or not new)
                                         --   this is the uniq id in the database
                         , reUniqId   :: Text -- Hash Id with concatenation of sha parameters
                         } deriving (Show, Generic)

instance FromRow ReturnId where
  fromRow = ReturnId <$> field <*> field <*> field

---------------------------------------------------------------------------
-- * Uniqueness of document definition

class AddUniqId a
  where
    addUniqId :: a -> a

-- instance AddUniqId HyperdataDocument
--   where
--     addUniqId = addUniqIdsDoc
--       where
--         addUniqIdsDoc :: HyperdataDocument -> HyperdataDocument
--         addUniqIdsDoc doc = set hd_uniqIdBdd (Just shaBdd)
--                           $ set hd_uniqId    (Just shaUni) doc
--           where
--             shaUni = hash $ DT.concat $ map ($ doc) shaParametersDoc
--             shaBdd = hash $ DT.concat $ map ($ doc) ([maybeText . _hd_bdd] <> shaParametersDoc)

--             shaParametersDoc :: [HyperdataDocument -> Text]
--             shaParametersDoc = [ filterText . maybeText . _hd_title
--                                , filterText . maybeText . _hd_abstract
--                                , filterText . maybeText . _hd_source
--                         --       , \d -> maybeText (_hd_publication_date d)
--                                ]

class UniqParameters a
  where
    uniqParameters :: a -> Text

instance UniqParameters HyperdataDocument
  where
    uniqParameters h = filterText $  DT.concat $ map maybeText $ [_hd_title h, _hd_abstract h, _hd_source h]

instance UniqParameters HyperdataContact
  where
    uniqParameters _ = ""

instance UniqParameters (Node a)
  where
    uniqParameters _ = undefined


filterText :: Text -> Text
filterText = DT.toLower . DT.filter isAlphaNum


instance (UniqParameters a, ToJSON a, HasDBid NodeType) => AddUniqId (Node a)
  where
    addUniqId (Node nid _ t u p n d h)  = Node nid (Just newHash) t u p n d h
      where
        newHash = "\\x" <> hash (uniqParameters h)


    ---------------------------------------------------------------------------
-- * Uniqueness of document definition
-- TODO factorize with above (use the function below for tests)

-- instance AddUniqId HyperdataContact
--   where
--     addUniqId = addUniqIdsContact

-- addUniqIdsContact :: HyperdataContact -> HyperdataContact
-- addUniqIdsContact hc = set hc_uniqIdBdd (Just shaBdd)
--                      $ set hc_uniqId    (Just shaUni) hc
--   where
--     shaUni = hash $ DT.concat $ map ($ hc) shaParametersContact
--     shaBdd = hash $ DT.concat $ map ($ hc) ([maybeText . view hc_bdd] <> shaParametersContact)

--     -- | TODO add more shaparameters
--     shaParametersContact :: [HyperdataContact -> Text]
--     shaParametersContact = [ maybeText . view (hc_who   . _Just . cw_firstName              )
--                            , maybeText . view (hc_who   . _Just . cw_lastName               )
--                            , maybeText . view (hc_where . _head . cw_touch . _Just . ct_mail)
--                            ]


maybeText :: Maybe Text -> Text
maybeText = maybe (DT.pack "") identity

---------------------------------------------------------------------------
class ToNode a
  where
    -- TODO Maybe NodeId
    toNode :: HasDBid NodeType => UserId -> Maybe ParentId -> a -> Node a

instance ToNode HyperdataDocument where
  toNode u p h = Node 0 Nothing (toDBid NodeDocument) u p n date h
    where
      n    = maybe "No Title" (DT.take 255) (_hd_title h)
      date  = jour y m d
      -- NOTE: There is no year '0' in postgres, there is year 1 AD and beofre that year 1 BC:
      -- select '0001-01-01'::date, '0001-01-01'::date - '1 day'::interval;
      -- 0001-01-01    0001-12-31 00:00:00 BC
      y = fromIntegral $ fromMaybe Defaults.day $ _hd_publication_year  h
      m = fromMaybe Defaults.month $ _hd_publication_month h
      d = fromMaybe (fromIntegral Defaults.year) $ _hd_publication_day   h

-- TODO better Node
instance ToNode HyperdataContact where
  toNode u p = Node 0 Nothing (toDBid NodeContact) u p "Contact" date
    where
      date  = jour 2020 01 01




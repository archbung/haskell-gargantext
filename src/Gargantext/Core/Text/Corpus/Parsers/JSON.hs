{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.JSON
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

JSON parser for Gargantext corpus files.

-}

{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Core.Text.Corpus.Parsers.JSON where

-- import Gargantext.Database.Schema.Node (NodePoly(..))
import Conduit
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import GHC.Generics
import Gargantext.Core (Lang)
import Gargantext.Core.Text.Corpus.Parsers.JSON.Istex qualified as Istex
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (length)
import Protolude


data JSONStruct =
  JSONStruct { documents    :: [ JSONStructDocument ]
             , garg_version :: Text }
  deriving (Generic)
instance FromJSON JSONStruct

data JSONStructDocument =
  JSONStructDocument { document :: JSONDocument
                     , ngrams   :: JSONNgrams
                     , hash     :: Text }
  deriving (Generic)
instance FromJSON JSONStructDocument

data JSONDocument =
  JSONDocument { id        :: Int
               , hash_id   :: Maybe Text
               , typename  :: Int
               , user_id   :: Int
               , parent_id :: Maybe Int
               , name      :: Text
               , date      :: Text
               , hyperdata :: HyperdataDocument }
  deriving (Generic)
instance FromJSON JSONDocument

data JSONNgrams =
  JSONNgrams { ngrams :: [Text]
             , hash   :: Text }
  deriving (Generic)
instance FromJSON JSONNgrams

------------------------------------------------------------------------
-- | TODO: documents -> document -> hyperdata + title etc
readJSONLazyBS :: (FromJSON a) => BL.ByteString -> Either Text a
readJSONLazyBS bs = first T.pack $ eitherDecode bs


parseJSONC :: BL.ByteString
           -> Either Text (Integer, ConduitT () HyperdataDocument Identity ())
parseJSONC bs = f <$> readJSONLazyBS bs
  where
    f (JSONStruct { documents }) =
      ( fromIntegral $ length documents
      , yieldMany documents .| mapC doc2hyperdoc )

doc2hyperdoc :: JSONStructDocument -> HyperdataDocument
doc2hyperdoc (JSONStructDocument { document = JSONDocument { hyperdata } }) = hyperdata


parseIstex :: Lang
           -> BL.ByteString
           -> IO (Either Text HyperdataDocument)
parseIstex l bs = do
  let ej = readJSONLazyBS bs
  case ej of
    Left err -> pure $ Left err
    Right j -> Right <$> Istex.toDoc l j

{-|
Module      : Gargantext.Database.Schema.NgramsPostag
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams connection to the Database.

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.Ngrams
  where

import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.ByteString.Char8 qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (splitOn, strip)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.FromField (returnError, ResultError(..))
import Gargantext.Core (HasDBid(..))
import Gargantext.Core.Text.Ngrams ( Ngrams(..), NgramsType(..), NgramsT )
import Gargantext.Core.Types (Typed(..))
import Gargantext.Database.Schema.Prelude hiding (over)
import Gargantext.Database.Types ( Indexed(Indexed) )
import Gargantext.Prelude


type NgramsId  = Int
type Size      = Int

data NgramsPoly id terms n = NgramsDB { _ngrams_id    :: !id
                                      , _ngrams_terms :: !terms
                                      , _ngrams_n     :: !n
                                      } deriving (Show)

type NgramsWrite = NgramsPoly (Maybe (Field SqlInt4))
                                   (Field SqlText)
                                   (Field SqlInt4)

type NgramsRead  = NgramsPoly (Field SqlInt4)
                              (Field SqlText)
                              (Field SqlInt4)

type NgramsDB = NgramsPoly Int Text Int

$(makeAdaptorAndInstance "pNgramsDb"    ''NgramsPoly)
makeLenses ''NgramsPoly


ngramsTable :: Table NgramsWrite NgramsRead
ngramsTable = Table "ngrams" (pNgramsDb NgramsDB { _ngrams_id    = optionalTableField "id"
                                                 , _ngrams_terms = requiredTableField "terms"
                                                 , _ngrams_n     = requiredTableField "n"
                                                 }
                              )

-- map NgramsType to its assigned id
instance FromField NgramsType where
  fromField fld mdata =
    case B.unpack `fmap` mdata of
      Nothing -> returnError UnexpectedNull fld ""
      Just dat -> do
        n <- fromField fld mdata
        if (n :: Int) > 0 then
          case fromNgramsTypeId (NgramsTypeId n) of
            Nothing -> returnError ConversionFailed fld dat
            Just nt -> pure nt
        else
          returnError ConversionFailed fld dat
instance ToField NgramsType where
  toField nt = toField $ toDBid nt

instance FromField Ngrams where
  fromField fld mdata = do
    x <- fromField fld mdata
    pure $ text2ngrams x

instance PGS.ToRow Text where
  toRow t = [toField t]

text2ngrams :: Text -> Ngrams
text2ngrams txt = UnsafeNgrams txt' $ length $ splitOn " " txt'
  where
    txt' = strip txt


newtype NgramsTypeId = NgramsTypeId Int
  deriving (Eq, Show, Ord, Num)
instance ToField NgramsTypeId where
  toField (NgramsTypeId n) = toField n
instance FromField NgramsTypeId where
  fromField fld mdata = do
    n <- fromField fld mdata
    if (n :: Int) > 0 then pure $ NgramsTypeId n
                      else mzero
instance DefaultFromField (Nullable SqlInt4) NgramsTypeId
  where
    defaultFromField = fromPGSFromField

pgNgramsType :: NgramsType -> Field SqlInt4
pgNgramsType = pgNgramsTypeId . NgramsTypeId . toDBid

pgNgramsTypeId :: NgramsTypeId -> Field SqlInt4
pgNgramsTypeId (NgramsTypeId n) = sqlInt4 n

-- | Bidirectional map between an 'NgramsType' and its id.
-- /NOTE/ This function is total in its domain by construction.
ngramsTypeIds :: Bimap NgramsType NgramsTypeId
ngramsTypeIds = Bimap.fromList $ [minBound .. maxBound] <&> \nt -> case nt of
  Authors     -> (nt, 1)
  Institutes  -> (nt, 2)
  Sources     -> (nt, 3)
  NgramsTerms -> (nt, 4)

fromNgramsTypeId :: NgramsTypeId -> Maybe NgramsType
fromNgramsTypeId nid = Bimap.lookupR nid ngramsTypeIds

unNgramsTypeId :: NgramsTypeId -> Int
unNgramsTypeId (NgramsTypeId i) = i

toNgramsTypeId :: Int -> NgramsTypeId
toNgramsTypeId i = NgramsTypeId i

instance HasDBid NgramsType where
  toDBid nt  = unNgramsTypeId $ ngramsTypeIds Bimap.! nt -- cannot fail
  lookupDBid = fromNgramsTypeId . toNgramsTypeId

------------------------------------------------------------------------
------------------------------------------------------------------------
-----------------------------------------------------------------------
withMap :: HashMap Text NgramsId -> Text -> NgramsId
withMap m n = maybe (panicTrace $ "[G.D.S.Ngrams.withMap] Should not happen" <> (show n))
                    identity (HashMap.lookup n m)

indexNgramsT :: HashMap Text NgramsId -> NgramsT Ngrams -> NgramsT (Indexed Int Ngrams)
indexNgramsT = fmap . indexNgramsWith . withMap

-- | TODO replace NgramsT whith Typed NgramsType Ngrams
indexTypedNgrams :: HashMap Text NgramsId
                 -> Typed NgramsType Ngrams
                 -> Typed NgramsType (Indexed Int Ngrams)
indexTypedNgrams = fmap . indexNgramsWith . withMap

indexNgrams :: HashMap Text NgramsId -> Ngrams -> Indexed Int Ngrams
indexNgrams = indexNgramsWith . withMap

indexNgramsWith :: (Text -> NgramsId) -> Ngrams -> Indexed Int Ngrams
indexNgramsWith f n = Indexed (f $ _ngramsTerms n) n

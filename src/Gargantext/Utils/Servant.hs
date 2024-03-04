{-|
Module      : Gargantext.Utils.Servant
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Utils.Servant where

import Data.ByteString.Lazy.Char8 qualified as BSC
import Data.Csv (encodeDefaultOrderedByName, DefaultOrdered, ToNamedRecord)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Media ((//), (/:))
import Prelude qualified
import Protolude
import Protolude.Partial (read)
import Servant ( Accept(contentType), MimeRender(..), MimeUnrender(mimeUnrender) )


data CSV = CSV

instance Accept CSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance (DefaultOrdered a, ToNamedRecord a) => MimeRender CSV [a] where
  mimeRender _ = encodeDefaultOrderedByName

instance MimeRender CSV T.Text where
  mimeRender _ = BSC.fromStrict . TE.encodeUtf8

instance Read a => MimeUnrender CSV a where
   mimeUnrender _ bs = case BSC.take len bs of
     "text/csv" -> pure . read . BSC.unpack $ BSC.drop len bs
     _ -> Left "didn't start with the magic incantation"
     where
       len :: Int64
       len = fromIntegral $ length ("text/csv" :: Prelude.String)

--instance ToNamedRecord a => MimeRender CSV [a] where
--  mimeRender _ val = encode val

----------------------------

data Markdown = Markdown

instance Accept Markdown where
  contentType _ = "text" // "markdown"

instance MimeRender Markdown T.Text where
  mimeRender _ = BSC.fromStrict . TE.encodeUtf8

instance MimeUnrender Markdown T.Text where
  mimeUnrender _ = Right . TE.decodeUtf8 . BSC.toStrict


---------------------------

data ZIP = ZIP

instance Accept ZIP where
  contentType _ = "application" // "zip"

instance MimeRender ZIP BSC.ByteString where
  mimeRender _ = identity

instance MimeUnrender ZIP BSC.ByteString where
  mimeUnrender _ = Right . identity


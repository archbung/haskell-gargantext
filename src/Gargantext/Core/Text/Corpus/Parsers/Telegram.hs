module Gargantext.Core.Text.Corpus.Parsers.Telegram
  where

import Data.Aeson
import Data.Text (Text)
--import Data.Time
import GHC.Generics (Generic)
--import Gargantext.Core (Lang(..))
--import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import System.FilePath (FilePath)
import qualified Data.ByteString.Lazy as DBL

readFile_Telegram :: FilePath -> IO [TelegramMsg]
readFile_Telegram fp = do
  raw <- DBL.readFile fp
  let mayIssues = decode raw
  case mayIssues of
    Just is -> pure is
    Nothing -> pure []


data TelegramMsg = TelegramMsg { _action_entities :: !Text
                   , _broadcastg :: !Text
                   , _buttonsg :: !Text
                   , _buttons_countg :: !Text
                   , _buttons_flatg :: !Text
                   , _chatg :: !Text
                   , _chat_peerg :: !Text
                   , _fileg :: !Text
                   , _forwardg :: !Text
                   , _input_chatg :: !Text
                   , _input_senderg :: !Text
                   , _linked_chatg :: !Text
                   , _reply_messageg :: !Text
                   , _senderg :: !Text
                   , _sender_idg :: !Text
                   , _textg :: !Text
                   , _via_botg :: !Text
                   , _via_input_botg :: !Text
                   , actiong :: !Text
                   , dateg :: !Text
                   , edit_dateg :: !Text
                   , edit_hideg :: !Text
                   , entitiesg :: !Text
                   , forwardsg :: !Text
                   , from_idg :: !Text
                   , from_scheduledg :: !Text
                   , fwd_fromg :: !Text
                   , grouped_idg :: !Text
                   , idg :: !Text
                   , legacyg :: !Text
                   , mediag :: !Text
                   , media_unreadg :: !Text
                   , mentionedg :: !Text
                   , messageg :: !Text
                   , noforwardsg :: !Text
                   , outg :: !Text
                   , peer_idg :: !Text
                   , pinnedg :: !Text
                   , postg :: !Text
                   , post_authorg :: !Text
                   , reactionsg :: !Text
                   , repliesg :: !Text
                   , reply_markupg :: !Text
                   , reply_tog :: !Text
                   , restriction_reasong :: !Text
                   , silentg :: !Text
                   , ttl_periodg :: !Text
                   , via_bot_idg :: !Text
                   , views :: !Text
                  }
  deriving (Show, Generic)

instance FromJSON TelegramMsg

{-
gitlabIssue2hyperdataDocument :: Issue -> HyperdataDocument
gitlabIssue2hyperdataDocument issue = HyperdataDocument
    { _hd_bdd = Nothing
    , _hd_doi = Nothing
    , _hd_url = Nothing
    , _hd_uniqId = Nothing
    , _hd_uniqIdBdd = Nothing
    , _hd_page = Nothing
    , _hd_title = Just (_issue_title issue)
    , _hd_authors = Nothing
    , _hd_institutes = Nothing
    , _hd_source = Nothing
    , _hd_abstract = Just (_issue_content issue)
    , _hd_publication_date = Just $ DT.pack $ show date
    , _hd_publication_year = Just $ fromIntegral year
    , _hd_publication_month = Just month
    , _hd_publication_day = Just day
    , _hd_publication_hour = Just (todHour tod)
    , _hd_publication_minute = Just (todMin tod)
    , _hd_publication_second = Just (round $ todSec tod)
    , _hd_language_iso2 = Just $ (DT.pack . show) lang
    }
  where lang = EN
        date = _issue_created issue
        (year, month, day) = toGregorian $ localDay date
        tod = localTimeOfDay date
-}

{-
readFile_IssuesAsDocs :: FilePath -> IO [HyperdataDocument]
readFile_IssuesAsDocs = fmap (fmap gitlabIssue2hyperdataDocument) . readFile_Issues
-}

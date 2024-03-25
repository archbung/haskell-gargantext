{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- TODO-ACCESS:
--   check userId       CanFillUserCorpus   userCorpusId
--   check masterUserId CanFillMasterCorpus masterCorpusId

-- TODO-ACCESS: check uId CanInsertDoc pId && checkDocType nodeType
-- TODO-EVENTS: InsertedNodes
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators           #-}

module Gargantext.Database.Action.Flow -- (flowDatabase, ngrams2list)
  ( DataText(..)
  , getDataText
  , getDataText_Debug
  , flowDataText
  , flow

  , flowCorpusFile
  , flowCorpus
  , flowCorpusUser
  , flowAnnuaire
  , insertMasterDocs
  , saveDocNgramsWith
  , addDocumentsToHyperCorpus

  , reIndexWith

  , getOrMkRoot
  , getOrMkRootWithCorpus
  , TermType(..)
  , DataOrigin(..)
  , allDataOrigins

  , do_api
  )
    where

import Conduit
import Control.Lens ( (^.), to, view, over )
import Data.Bifunctor qualified as B
import Data.Conduit qualified as C
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as CL
import Data.Conduit.List qualified as CList
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import EPO.API.Client.Types qualified as EPO
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.Core (Lang(..), NLPServerConfig, withDefaultLanguage)
import Gargantext.Core.Ext.IMTUser (readFile_Annuaire)
import Gargantext.Core.NLP (HasNLPServer, nlpServerGet)
import Gargantext.Core.NodeStory.Types (HasNodeStory)
import Gargantext.Core.Text.Corpus.API qualified as API
import Gargantext.Core.Text.Corpus.Parsers (parseFile, FileFormat, FileType)
import Gargantext.Core.Text.List (buildNgramsLists)
import Gargantext.Core.Text.List.Group.WithStem (GroupParams(..))
import Gargantext.Core.Text.List.Social (FlowSocialListWith(..))
import Gargantext.Core.Text.Ngrams (NgramsType(NgramsTerms), Ngrams(_ngramsTerms))
import Gargantext.Core.Text.Terms
import Gargantext.Core.Text.Terms.Mono.Stem (stem, StemmingAlgorithm(..))
import Gargantext.Core.Types (HasValidationError, TermsCount)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main ( ListType(MapTerm) )
import Gargantext.Core.Types.Query (Limit)
import Gargantext.Database.Action.Flow.Extract ()  -- ExtractNgramsT instances
import Gargantext.Database.Action.Flow.List ( flowList_DbRepo, toNodeNgramsW' )
import Gargantext.Database.Action.Flow.Types ( do_api, DataOrigin(..), DataText(..), FlowCorpus )
import Gargantext.Database.Action.Flow.Utils (docNgrams, documentIdWithNgrams, insertDocNgrams, insertDocs, mapNodeIdNgrams)
import Gargantext.Database.Action.Metrics (updateNgramsOccurrences, updateContextScore)
import Gargantext.Database.Action.Search (searchDocInDatabase)
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact )
import Gargantext.Database.Admin.Types.Hyperdata.Corpus ( HyperdataAnnuaire, HyperdataCorpus(_hc_lang) )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( ToHyperdataDocument(toHyperdataDocument) )
import Gargantext.Database.Admin.Types.Node hiding (DEBUG) -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Prelude (DbCmd', DBCmd, hasConfig)
import Gargantext.Database.Query.Table.ContextNodeNgrams2 ( ContextNodeNgrams2Poly(..), insertContextNodeNgrams2 )
import Gargantext.Database.Query.Table.Node ( MkCorpus, insertDefaultNodeIfNotExists, getOrMkList, getNodeWith )
import Gargantext.Database.Query.Table.Node.Document.Add qualified as Doc (add)
import Gargantext.Database.Query.Table.Node.Document.Insert ( ToNode(toNode) ) -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Query.Table.NodeNgrams (listInsertDb , getCgramsId)
import Gargantext.Database.Query.Tree.Root (MkCorpusUser(..), getOrMkRoot, getOrMkRootWithCorpus, userFromMkCorpusUser)
import Gargantext.Database.Schema.Ngrams ( indexNgrams, text2ngrams )
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude hiding (to)
import Gargantext.Prelude.Config (GargConfig(..))
import Gargantext.System.Logging ( logLocM, LogLevel(DEBUG), MonadLogger )
import Gargantext.Utils.Jobs.Monad ( JobHandle, MonadJobStatus(..) )
import PUBMED.Types qualified as PUBMED

------------------------------------------------------------------------
-- Imports for upgrade function
import Gargantext.Database.Query.Tree.Error ( HasTreeError )

------------------------------------------------------------------------

allDataOrigins :: [DataOrigin]
allDataOrigins = map InternalOrigin API.externalAPIs <> map ExternalOrigin API.externalAPIs

---------------

-- Show instance is not possible because of IO
printDataText :: DataText -> IO ()
printDataText (DataOld xs) = putText $ show xs
printDataText (DataNew (maybeInt, conduitData)) = do
  res <- C.runConduit (conduitData .| CL.consume)
  putText $ show (maybeInt, res)

-- TODO use the split parameter in config file
getDataText :: (HasNodeError err)
            => DataOrigin
            -> TermType Lang
            -> API.RawQuery
            -> Maybe PUBMED.APIKey
            -> Maybe EPO.AuthKey
            -> Maybe API.Limit
            -> DBCmd err (Either API.GetCorpusError DataText)
getDataText (ExternalOrigin api) la q mPubmedAPIKey mAuthKey li = do
  cfg <- view hasConfig
  eRes <- liftBase $ API.get api (_tt_lang la) q mPubmedAPIKey mAuthKey (_gc_epo_api_url cfg) li
  pure $ DataNew <$> eRes
getDataText (InternalOrigin _) la q _ _ _li = do
  (_masterUserId, _masterRootId, cId) <- getOrMkRootWithCorpus MkCorpusUserMaster (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchDocInDatabase cId (stem (_tt_lang la) GargPorterAlgorithm $ API.getRawQuery q)
  pure $ Right $ DataOld ids

getDataText_Debug :: (HasNodeError err)
                  => DataOrigin
                  -> TermType Lang
                  -> API.RawQuery
                  -> Maybe API.Limit
                  -> DBCmd err ()
getDataText_Debug a l q li = do
  result <- getDataText a l q Nothing Nothing li
  case result of
    Left  err -> liftBase $ putText $ show err
    Right res -> liftBase $ printDataText res


-------------------------------------------------------------------------------
flowDataText :: forall env err m.
                ( DbCmd' env err m
                , HasNodeStory env err m
                , MonadLogger m
                , HasNLPServer env
                , HasTreeError err
                , HasValidationError err
                , MonadJobStatus m
                )
                => User
                -> DataText
                -> TermType Lang
                -> CorpusId
                -> Maybe FlowSocialListWith
                -> JobHandle m
                -> m CorpusId
flowDataText u (DataOld ids) tt cid mfslw _ = do
  $(logLocM) DEBUG $ T.pack $ "Found " <> show (length ids) <> " old node IDs"
  (_userId, userCorpusId, listId) <- createNodes (MkCorpusUserNormalCorpusIds u [cid]) corpusType
  _ <- Doc.add userCorpusId (map nodeId2ContextId ids)
  flowCorpusUser (_tt_lang tt) u userCorpusId listId corpusType mfslw
  where
    corpusType = Nothing :: Maybe HyperdataCorpus
flowDataText u (DataNew (mLen, txtC)) tt cid mfslw jobHandle = do
  $(logLocM) DEBUG $ T.pack $ "Found " <> show mLen <> " new documents to process"
  for_ (mLen <&> fromInteger) (`addMoreSteps` jobHandle)
  flowCorpus (MkCorpusUserNormalCorpusIds u [cid]) tt mfslw (fromMaybe 0 mLen, transPipe liftBase txtC) jobHandle

------------------------------------------------------------------------
-- TODO use proxy
flowAnnuaire :: ( DbCmd' env err m
                , HasNodeStory env err m
                , MonadLogger m
                , HasNLPServer env
                , HasTreeError err
                , HasValidationError err
                , MonadJobStatus m )
             => MkCorpusUser
             -> TermType Lang
             -> FilePath
             -> JobHandle m
             -> m AnnuaireId
flowAnnuaire mkCorpusUser l filePath jobHandle = do
  -- TODO Conduit for file
  docs <- liftBase (readFile_Annuaire filePath :: IO [HyperdataContact])
  flow (Nothing :: Maybe HyperdataAnnuaire) mkCorpusUser l Nothing (fromIntegral $ length docs, yieldMany docs) jobHandle

------------------------------------------------------------------------
flowCorpusFile :: ( DbCmd' env err m
                  , HasNodeStory env err m
                  , MonadLogger m
                  , HasNLPServer env
                  , HasTreeError err
                  , HasValidationError err
                  , MonadJobStatus m )
           => MkCorpusUser
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang
           -> FileType
           -> FileFormat
           -> FilePath
           -> Maybe FlowSocialListWith
           -> JobHandle m
           -> m CorpusId
flowCorpusFile mkCorpusUser _l la ft ff fp mfslw jobHandle = do
  eParsed <- liftBase $ parseFile ft ff fp
  case eParsed of
    Right parsed -> do
      flowCorpus mkCorpusUser la mfslw (fromIntegral $ length parsed, yieldMany parsed .| mapC toHyperdataDocument) jobHandle
      --let docs = splitEvery 500 $ take l parsed
      --flowCorpus u n la mfslw (yieldMany $ map (map toHyperdataDocument) docs) logStatus
    Left e       -> panicTrace $ "Error: " <> e

------------------------------------------------------------------------
-- | TODO improve the needed type to create/update a corpus
-- (For now, Either is enough)
flowCorpus :: ( DbCmd' env err m
              , HasNodeStory env err m
              , MonadLogger m
              , HasNLPServer env
              , HasTreeError err
              , HasValidationError err
              , FlowCorpus a
              , MonadJobStatus m )
           => MkCorpusUser
           -> TermType Lang
           -> Maybe FlowSocialListWith
           -> (Integer, ConduitT () a m ())
           -> JobHandle m
           -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flow :: forall env err m a c.
        ( DbCmd' env err m
        , HasNodeStory env err m
        , MonadLogger m
        , HasNLPServer env
        , HasTreeError err
        , HasValidationError err
        , FlowCorpus a
        , MkCorpus c
        , MonadJobStatus m
        )
        => Maybe c
        -> MkCorpusUser
        -> TermType Lang
        -> Maybe FlowSocialListWith
        -> (Integer, ConduitT () a m ())
        -> JobHandle m
        -> m CorpusId
flow c mkCorpusUser la mfslw (count, docsC) jobHandle = do
  (_userId, userCorpusId, listId) <- createNodes mkCorpusUser c
  -- TODO if public insertMasterDocs else insertUserDocs
  nlpServer <- view $ nlpServerGet (_tt_lang la)
  runConduit $ zipSources (yieldMany ([1..] :: [Int])) docsC
    .| CList.chunksOf 100
    .| mapM_C (addDocumentsWithProgress nlpServer userCorpusId)
    .| sinkNull

  let u = userFromMkCorpusUser mkCorpusUser
    
  $(logLocM) DEBUG "Calling flowCorpusUser"
  flowCorpusUser (la ^. tt_lang) u userCorpusId listId c mfslw

  where
    addDocumentsWithProgress :: NLPServerConfig -> CorpusId -> [(Int, a)] -> m ()
    addDocumentsWithProgress nlpServer userCorpusId docsChunk = do
      $(logLocM) DEBUG $ T.pack $ "calling insertDoc, ([idx], mLength) = " <> show (fst <$> docsChunk, count)
      docs <- addDocumentsToHyperCorpus nlpServer c la userCorpusId (map snd docsChunk)
      markProgress (length docs) jobHandle


-- | Given a list of corpus documents and a 'NodeId' identifying the 'CorpusId', adds
-- the given documents to the corpus. Returns the Ids of the inserted documents.
addDocumentsToHyperCorpus :: ( DbCmd' env err m
                             , HasNodeError err
                             , FlowCorpus document
                             , MkCorpus corpus
                             )
                             => NLPServerConfig
                             -> Maybe corpus
                             -> TermType Lang
                             -> CorpusId
                             -> [document]
                             -> m [DocId]
addDocumentsToHyperCorpus ncs mb_hyper la corpusId docs = do
  ids <- insertMasterDocs ncs mb_hyper la docs
  void $ Doc.add corpusId (map nodeId2ContextId ids)
  pure ids

------------------------------------------------------------------------
createNodes :: ( DbCmd' env err m, HasNodeError err
               , MkCorpus c
               )
            => MkCorpusUser
            -> Maybe c
            -> m (UserId, CorpusId, ListId)
createNodes mkCorpusUser ctype = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMkRootWithCorpus mkCorpusUser ctype
  -- NodeTexts is first
  _tId <- insertDefaultNodeIfNotExists NodeTexts userCorpusId userId
  -- printDebug "NodeTexts: " tId

  -- NodeList is second
  listId <- getOrMkList userCorpusId userId

  -- User Graph Flow
  _ <- insertDefaultNodeIfNotExists NodeGraph     userCorpusId userId
  -- _ <- insertDefaultNodeIfNotExists NodeDashboard userCorpusId userId

  pure (userId, userCorpusId, listId)


flowCorpusUser :: ( HasNodeError err
                  , HasValidationError err
                  , HasNLPServer env
                  , HasTreeError err
                  , HasNodeStory env err m
                  , MkCorpus c
                  )
               => Lang
               -> User
               -> CorpusId
               -> ListId
               -> Maybe c
               -> Maybe FlowSocialListWith
               -> m CorpusId
flowCorpusUser l user userCorpusId listId ctype mfslw = do
  buildSocialList l user userCorpusId listId ctype mfslw

  -- _ <- insertOccsUpdates userCorpusId mastListId
  --_ <- mkPhylo  userCorpusId userId
  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  _ <- reIndexWith userCorpusId listId NgramsTerms (Set.singleton MapTerm)
  _ <- updateContextScore      userCorpusId listId
  _ <- updateNgramsOccurrences userCorpusId listId

  pure userCorpusId


-- | This function is responsible for contructing terms.
buildSocialList :: ( HasNodeError err
                   , HasValidationError err
                   , HasNLPServer env
                   , HasTreeError err
                   , HasNodeStory env err m
                   , MkCorpus c
                   )
                => Lang
                -> User
                -> CorpusId
                -> ListId
                -> Maybe c
                -> Maybe FlowSocialListWith
                -> m ()
buildSocialList _l _user _userCorpusId _listId _ctype (Just (NoList _)) = pure ()
buildSocialList l user userCorpusId listId ctype mfslw = do
  -- User List Flow
  (masterUserId, _masterRootId, masterCorpusId)
    <- getOrMkRootWithCorpus MkCorpusUserMaster ctype

  nlpServer <- view (nlpServerGet l)
  --let gp = (GroupParams l 2 3 (StopSize 3))
  -- Here the PosTagAlgo should be chosen according to the Lang
  -- let gp = GroupParams { unGroupParams_lang = l
  --                      , unGroupParams_len = 10
  --                      , unGroupParams_limit = 10
  --                      , unGroupParams_stopSize = StopSize 10 }
  let gp = GroupWithPosTag l nlpServer HashMap.empty
  ngs  <- buildNgramsLists user userCorpusId masterCorpusId mfslw gp

  -- printDebug "flowCorpusUser:ngs" ngs

  _userListId <- flowList_DbRepo listId ngs
  _mastListId <- getOrMkList masterCorpusId masterUserId
  pure ()


insertMasterDocs :: ( DbCmd' env err m
                    , HasNodeError err
                    , FlowCorpus a
                    , MkCorpus   c
                    )
                 => NLPServerConfig
                 -> Maybe c
                 -> TermType Lang
                 -> [a]
                 -> m [DocId]
insertMasterDocs ncs c lang hs  =  do
  (masterUserId, _, masterCorpusId) <- getOrMkRootWithCorpus MkCorpusUserMaster c
  (ids', documentsWithId) <- insertDocs masterUserId masterCorpusId (map (toNode masterUserId Nothing) hs )
  _ <- Doc.add masterCorpusId ids'
  -- TODO
  -- create a corpus with database name (CSV or PubMed)
  -- add documents to the corpus (create node_node link)
  -- this will enable global database monitoring

  -- maps :: IO Map Ngrams (Map NgramsType (Map NodeId Int))
  mapNgramsDocs' :: HashMap.HashMap ExtractedNgrams (Map NgramsType (Map NodeId (Int, TermsCount)))
                <- mapNodeIdNgrams
                <$> documentIdWithNgrams
                      (extractNgramsT ncs $ withLang lang documentsWithId)
                      (map (B.first contextId2NodeId) documentsWithId)

  lId <- getOrMkList masterCorpusId masterUserId
  -- _ <- saveDocNgramsWith lId mapNgramsDocs'
  _ <- saveDocNgramsWith lId mapNgramsDocs'

  -- _cooc <- insertDefaultNode NodeListCooc lId masterUserId
  pure $ map contextId2NodeId ids'

saveDocNgramsWith :: (DbCmd' env err m)
                  => ListId
                  -> HashMap.HashMap ExtractedNgrams (Map NgramsType (Map NodeId (Int, TermsCount)))
                  -> m ()
saveDocNgramsWith lId mapNgramsDocs' = do
  --printDebug "[saveDocNgramsWith] mapNgramsDocs'" mapNgramsDocs'
  let mapNgramsDocsNoCount = over (traverse . traverse . traverse) fst mapNgramsDocs'
  terms2id <- insertExtractedNgrams $ HashMap.keys mapNgramsDocsNoCount

  let mapNgramsDocs = HashMap.mapKeys extracted2ngrams mapNgramsDocs'

  -- new
  mapCgramsId <- listInsertDb lId toNodeNgramsW'
               $ map (bimap _ngramsTerms Map.keys)
               $ HashMap.toList mapNgramsDocs

  --printDebug "saveDocNgramsWith" mapCgramsId
  -- insertDocNgrams
  let ngrams2insert =  catMaybes [ ContextNodeNgrams2 (nodeId2ContextId nId)
                                            <$> getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms'')
                                            <*> Just (fromIntegral w :: Double)
                       | (terms'', mapNgramsTypes)      <- HashMap.toList mapNgramsDocs
                       , (ngrams_type, mapNodeIdWeight) <- Map.toList mapNgramsTypes
                       , (nId, (w, _cnt))               <- Map.toList mapNodeIdWeight
                       ]
  -- printDebug "Ngrams2Insert" ngrams2insert
  _return <- insertContextNodeNgrams2 ngrams2insert

  -- to be removed
  _   <- insertDocNgrams lId $ HashMap.mapKeys (indexNgrams terms2id) mapNgramsDocs

  pure ()


------------------------------------------------------------------------



-- | Re-index documents of a corpus with ngrams in the list
reIndexWith :: ( HasNodeStory env err m )
            => CorpusId
            -> ListId
            -> NgramsType
            -> Set ListType
            -> m ()
reIndexWith cId lId nt lts = do
  -- printDebug "(cId,lId,nt,lts)" (cId, lId, nt, lts)
  corpus_node <- getNodeWith cId (Proxy @HyperdataCorpus)
  let corpusLang = withDefaultLanguage $ view (node_hyperdata . to _hc_lang) corpus_node

  -- Getting [NgramsTerm]
  ts <- List.concat
     <$> map (\(k,vs) -> k:vs)
     <$> HashMap.toList
     <$> getTermsWith identity [lId] nt lts

  -- Get all documents of the corpus
  docs <- selectDocNodes cId

  let
    -- fromListWith (<>)
    ngramsByDoc = map (HashMap.fromListWith (Map.unionWith (Map.unionWith (\(_a,b) (_a',b') -> (1,b+b')))))
                $ map (map (\((k, cnt), v) -> (SimpleNgrams (text2ngrams k), over (traverse . traverse) (\p -> (p, cnt)) v)))
                $ map (docNgrams corpusLang nt ts) docs

  -- Saving the indexation in database
  mapM_ (saveDocNgramsWith lId) ngramsByDoc
  pure ()

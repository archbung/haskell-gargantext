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

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , docNgrams

  , getOrMkRoot
  , getOrMk_RootWithCorpus
  , TermType(..)
  , DataOrigin(..)
  , allDataOrigins

  , do_api
  , indexAllDocumentsWithPosTag
  )
    where

import Conduit
import Control.Lens hiding (elements, Indexed)
import Data.Aeson.TH (deriveJSON)
import Data.Conduit qualified as C
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as CL
import Data.Conduit.List qualified as CList
import Data.Either
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict (lookup)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Proxy
import Data.Set qualified as Set
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.API.Ngrams.Types qualified as NT
import Gargantext.Core (Lang(..), PosTagAlgo(..), NLPServerConfig)
import Gargantext.Core (withDefaultLanguage)
import Gargantext.Core.Ext.IMTUser (readFile_Annuaire)
import Gargantext.Core.Flow.Types
import Gargantext.Core.NLP (nlpServerGet)
import Gargantext.Core.NodeStory (HasNodeStory)
import Gargantext.Core.Text
import Gargantext.Core.Text.Corpus.API qualified as API
import Gargantext.Core.Text.Corpus.Parsers (parseFile, FileFormat, FileType, splitOn)
import Gargantext.Core.Text.List (buildNgramsLists)
import Gargantext.Core.Text.List.Group.WithStem ({-StopSize(..),-} GroupParams(..))
import Gargantext.Core.Text.List.Social (FlowSocialListWith(..))
import Gargantext.Core.Text.Terms
import Gargantext.Core.Text.Terms.Mono.Stem.En (stemIt)
import Gargantext.Core.Text.Terms.WithList (MatchedText, buildPatternsWith, termsInText)
import Gargantext.Core.Types (POS(NP), TermsCount)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main
import Gargantext.Core.Types.Query (Limit)
import Gargantext.Core.Utils (addTuples)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Data.HashMap.Strict.Utils qualified as HashMap
import Gargantext.Database.Action.Flow.List
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Flow.Utils (insertDocNgrams, DocumentIdWithNgrams(..))
import Gargantext.Database.Action.Metrics (updateNgramsOccurrences, updateContextScore)
import Gargantext.Database.Action.Search (searchDocInDatabase)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node hiding (DEBUG) -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.ContextNodeNgrams2
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Document.Add qualified as Doc (add)
import Gargantext.Database.Query.Table.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Query.Table.NodeNgrams (listInsertDb , getCgramsId)
import Gargantext.Database.Query.Tree.Root (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node (NodePoly(..), node_id)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Database.Types
import Gargantext.Prelude hiding (show)
import Gargantext.Prelude.Crypto.Hash (Hash)
import Gargantext.System.Logging
import Gargantext.Utils.Jobs (JobHandle, MonadJobStatus(..))
import Protolude hiding (to)
import PUBMED.Types qualified as PUBMED

------------------------------------------------------------------------
-- Imports for upgrade function
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Query.Tree (findNodesId)

------------------------------------------------------------------------
-- TODO use internal with API name (could be old data)
data DataOrigin = InternalOrigin { _do_api :: API.ExternalAPIs }
                | ExternalOrigin { _do_api :: API.ExternalAPIs }
               -- TODO Web
  deriving (Generic, Eq)

makeLenses ''DataOrigin
deriveJSON (unPrefix "_do_") ''DataOrigin
instance ToSchema DataOrigin where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_do_")

allDataOrigins :: [DataOrigin]
allDataOrigins = map InternalOrigin API.externalAPIs <> map ExternalOrigin API.externalAPIs

---------------
data DataText = DataOld ![NodeId]
              | DataNew !(Maybe Integer, ConduitT () HyperdataDocument IO ())
              --- | DataNew ![[HyperdataDocument]]

-- Show instance is not possible because of IO
printDataText :: DataText -> IO ()
printDataText (DataOld xs) = putText $ show xs
printDataText (DataNew (maybeInt, conduitData)) = do
  res <- C.runConduit (conduitData .| CL.consume)
  putText $ show (maybeInt, res)

-- TODO use the split parameter in config file
getDataText :: FlowCmdM env err m
            => DataOrigin
            -> TermType Lang
            -> API.RawQuery
            -> Maybe PUBMED.APIKey
            -> Maybe API.Limit
            -> m (Either API.GetCorpusError DataText)
getDataText (ExternalOrigin api) la q mPubmedAPIKey li = do
  eRes <- liftBase $ API.get api (_tt_lang la) q mPubmedAPIKey li
  pure $ DataNew <$> eRes
getDataText (InternalOrigin _) _la q _ _li = do
  (_masterUserId, _masterRootId, cId) <- getOrMk_RootWithCorpus
                                           (UserName userMaster)
                                           (Left "")
                                           (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchDocInDatabase cId (stemIt $ API.getRawQuery q)
  pure $ Right $ DataOld ids

getDataText_Debug :: FlowCmdM env err m
            => DataOrigin
            -> TermType Lang
            -> API.RawQuery
            -> Maybe API.Limit
            -> m ()
getDataText_Debug a l q li = do
  result <- getDataText a l q Nothing li
  case result of
    Left  err -> liftBase $ putText $ show err
    Right res -> liftBase $ printDataText res


-------------------------------------------------------------------------------
flowDataText :: forall env err m.
                ( FlowCmdM env err m
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
  (_userId, userCorpusId, listId) <- createNodes u (Right [cid]) corpusType
  _ <- Doc.add userCorpusId ids
  flowCorpusUser (_tt_lang tt) u userCorpusId listId corpusType mfslw
  where
    corpusType = (Nothing :: Maybe HyperdataCorpus)
flowDataText u (DataNew (mLen, txtC)) tt cid mfslw jobHandle = do
  $(logLocM) DEBUG $ T.pack $ "Found " <> show mLen <> " new documents to process"
  for_ (mLen <&> fromInteger) (`addMoreSteps` jobHandle)
  flowCorpus u (Right [cid]) tt mfslw (fromMaybe 0 mLen, (transPipe liftBase txtC)) jobHandle

------------------------------------------------------------------------
-- TODO use proxy
flowAnnuaire :: (FlowCmdM env err m, MonadJobStatus m)
             => User
             -> Either CorpusName [CorpusId]
             -> (TermType Lang)
             -> FilePath
             -> JobHandle m
             -> m AnnuaireId
flowAnnuaire u n l filePath jobHandle = do
  -- TODO Conduit for file
  docs <- liftBase $ ((readFile_Annuaire filePath) :: IO [HyperdataContact])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l Nothing (fromIntegral $ length docs, yieldMany docs) jobHandle

------------------------------------------------------------------------
flowCorpusFile :: (FlowCmdM env err m, MonadJobStatus m)
           => User
           -> Either CorpusName [CorpusId]
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang
           -> FileType
           -> FileFormat
           -> FilePath
           -> Maybe FlowSocialListWith
           -> JobHandle m
           -> m CorpusId
flowCorpusFile u n _l la ft ff fp mfslw jobHandle = do
  eParsed <- liftBase $ parseFile ft ff fp
  case eParsed of
    Right parsed -> do
      flowCorpus u n la mfslw (fromIntegral $ length parsed, yieldMany parsed .| mapC toHyperdataDocument) jobHandle
      --let docs = splitEvery 500 $ take l parsed
      --flowCorpus u n la mfslw (yieldMany $ map (map toHyperdataDocument) docs) logStatus
    Left e       -> panic $ "Error: " <> e

------------------------------------------------------------------------
-- | TODO improve the needed type to create/update a corpus
-- (For now, Either is enough)
flowCorpus :: (FlowCmdM env err m, FlowCorpus a, MonadJobStatus m)
           => User
           -> Either CorpusName [CorpusId]
           -> TermType Lang
           -> Maybe FlowSocialListWith
           -> (Integer, ConduitT () a m ())
           -> JobHandle m
           -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flow :: forall env err m a c.
        ( FlowCmdM env err m
        , FlowCorpus a
        , MkCorpus c
        , MonadJobStatus m
        )
        => Maybe c
        -> User
        -> Either CorpusName [CorpusId]
        -> TermType Lang
        -> Maybe FlowSocialListWith
        -> (Integer, ConduitT () a m ())
        -> JobHandle m
        -> m CorpusId
flow c u cn la mfslw (count, docsC) jobHandle = do
  (_userId, userCorpusId, listId) <- createNodes u cn c
  -- TODO if public insertMasterDocs else insertUserDocs
  nlpServer <- view $ nlpServerGet (_tt_lang la)
  runConduit $ zipSources (yieldMany ([1..] :: [Int])) docsC
                 .| CList.chunksOf 100
                 .| mapM_C (addDocumentsWithProgress nlpServer userCorpusId)
                 .| sinkNull

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
  void $ Doc.add corpusId ids
  pure ids

------------------------------------------------------------------------
createNodes :: ( DbCmd' env err m, HasNodeError err
               , MkCorpus c
               )
            => User
            -> Either CorpusName [CorpusId]
            -> Maybe c
            -> m (UserId, CorpusId, ListId)
createNodes user corpusName ctype = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMk_RootWithCorpus user corpusName ctype
  -- NodeTexts is first
  _tId <- insertDefaultNodeIfNotExists NodeTexts userCorpusId userId
  -- printDebug "NodeTexts: " tId

  -- NodeList is second
  listId <- getOrMkList userCorpusId userId

  -- User Graph Flow
  _ <- insertDefaultNodeIfNotExists NodeGraph     userCorpusId userId
  -- _ <- insertDefaultNodeIfNotExists NodeDashboard userCorpusId userId

  pure (userId, userCorpusId, listId)


flowCorpusUser :: ( FlowCmdM env err m
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
  server <- view (nlpServerGet l)
  -- User List Flow
  (masterUserId, _masterRootId, masterCorpusId)
    <- getOrMk_RootWithCorpus (UserName userMaster) (Left "") ctype

  --let gp = (GroupParams l 2 3 (StopSize 3))
  -- Here the PosTagAlgo should be chosen according to the Lang
  _ <- case mfslw of
         (Just (NoList _)) -> do
           -- printDebug "Do not build list" mfslw
           pure ()
         _ -> do
           ngs  <- buildNgramsLists user userCorpusId masterCorpusId mfslw
                   $ GroupWithPosTag l server HashMap.empty

         -- printDebug "flowCorpusUser:ngs" ngs

           _userListId <- flowList_DbRepo listId ngs
           _mastListId <- getOrMkList masterCorpusId masterUserId
           pure ()
  -- _ <- insertOccsUpdates userCorpusId mastListId
  --_ <- mkPhylo  userCorpusId userId
  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  _ <- reIndexWith userCorpusId listId NgramsTerms (Set.singleton MapTerm)
  _ <- updateContextScore      userCorpusId listId
  _ <- updateNgramsOccurrences userCorpusId listId
  
  pure userCorpusId


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
  (masterUserId, _, masterCorpusId) <- getOrMk_RootWithCorpus (UserName userMaster) (Left corpusMasterName) c
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
                      documentsWithId

  lId      <- getOrMkList masterCorpusId masterUserId
  -- _ <- saveDocNgramsWith lId mapNgramsDocs'
  _ <- saveDocNgramsWith lId mapNgramsDocs'

  -- _cooc <- insertDefaultNode NodeListCooc lId masterUserId
  pure ids'

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
               $ map (first _ngramsTerms . second Map.keys)
               $ HashMap.toList mapNgramsDocs

  --printDebug "saveDocNgramsWith" mapCgramsId
  -- insertDocNgrams
  let ngrams2insert =  catMaybes [ ContextNodeNgrams2 <$> Just nId
                                            <*> (getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms''))
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
-- TODO Type NodeDocumentUnicised
insertDocs :: ( DbCmd' env err m
              -- , FlowCorpus a
              , FlowInsertDB a
              , HasNodeError err
              )
              => UserId
              -> CorpusId
              -> [a]
              -> m ([ContextId], [Indexed ContextId a])
insertDocs uId cId hs = do
  let docs = map addUniqId hs
  newIds <- insertDb uId Nothing docs
  -- printDebug "newIds" newIds
  let
    newIds' = map reId newIds
    documentsWithId = mergeData (toInserted newIds) (Map.fromList $ map viewUniqId' docs)
  _ <- Doc.add cId newIds'
  pure (newIds', documentsWithId)


------------------------------------------------------------------------
viewUniqId' :: UniqId a
            => a
            -> (Hash, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId]
           -> Map Hash ReturnId
toInserted =
  Map.fromList . map    (\r -> (reUniqId r, r)     )
               . filter (\r -> reInserted r == True)

mergeData :: Map Hash ReturnId
          -> Map Hash a
          -> [Indexed NodeId a]
mergeData rs = catMaybes . map toDocumentWithId . Map.toList
  where
    toDocumentWithId (sha,hpd) =
      Indexed <$> fmap reId (lookup sha rs)
              <*> Just hpd

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
documentIdWithNgrams :: HasNodeError err
                     => (a
                     -> DBCmd err (HashMap.HashMap b (Map NgramsType Int, TermsCount)))
                     -> [Indexed NodeId a]
                     -> DBCmd err [DocumentIdWithNgrams a b]
documentIdWithNgrams f = traverse toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ _unIndex         d
      pure $ DocumentIdWithNgrams d e


-- | TODO check optimization
mapNodeIdNgrams :: (Ord b, Hashable b)
                => [DocumentIdWithNgrams a b]
                -> HashMap.HashMap b
                       (Map NgramsType
                            (Map NodeId (Int, TermsCount))
                       )
mapNodeIdNgrams = HashMap.unionsWith (Map.unionWith (Map.unionWith addTuples)) . fmap f
  where
    -- | NOTE We are somehow multiplying 'TermsCount' here: If the
    -- same ngrams term has different ngrams types, the 'TermsCount'
    -- for it (which is the number of times the terms appears in a
    -- document) is copied over to all its types.
    f :: DocumentIdWithNgrams a b
      -> HashMap.HashMap b (Map NgramsType (Map NodeId (Int, TermsCount)))
    f d = fmap (\(ngramsTypeMap, cnt) -> fmap (\i -> Map.singleton nId (i, cnt)) ngramsTypeMap) $ documentNgrams d
      where
        nId = _index $ documentWithId d


------------------------------------------------------------------------
instance ExtractNgramsT HyperdataContact
  where
    extractNgramsT _ncs l hc = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extract l hc
      where
        extract :: TermType Lang -> HyperdataContact
                -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
        extract _l hc' = do
          let authors = map text2ngrams
                      $ maybe ["Nothing"] (\a -> [a])
                      $ view (hc_who . _Just . cw_lastName) hc'

          pure $ HashMap.fromList $ [(SimpleNgrams a', (Map.singleton Authors 1, 1)) | a' <- authors ]


instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT :: NLPServerConfig
                   -> TermType Lang
                   -> HyperdataDocument
                   -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
    extractNgramsT ncs lang hd = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extractNgramsT' hd
      where
        extractNgramsT' :: HyperdataDocument
                        -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
        extractNgramsT' doc = do
          let source    = text2ngrams
                        $ maybe "Nothing" identity
                        $ _hd_source doc

              institutes = map text2ngrams
                         $ maybe ["Nothing"] (splitOn Institutes (doc^. hd_bdd))
                         $ _hd_institutes doc

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (splitOn Authors (doc^. hd_bdd))
                         $ _hd_authors doc

          termsWithCounts' <- map (\(t, cnt) -> (enrichedTerms (lang ^. tt_lang) CoreNLP NP t, cnt))
                              <$> concat
                              <$> liftBase (extractTerms ncs lang $ hasText doc)

          pure $ HashMap.fromList
               $  [(SimpleNgrams source, (Map.singleton Sources     1, 1))                    ]
               <> [(SimpleNgrams     i', (Map.singleton Institutes  1, 1)) | i' <- institutes ]
               <> [(SimpleNgrams     a', (Map.singleton Authors     1, 1)) | a' <- authors    ]
               <> [(EnrichedNgrams   t', (Map.singleton NgramsTerms 1, cnt')) | (t', cnt') <- termsWithCounts'     ]

instance (ExtractNgramsT a, HasText a) => ExtractNgramsT (Node a)
  where
    extractNgramsT ncs l (Node { _node_hyperdata = h }) = extractNgramsT ncs l h

instance HasText a => HasText (Node a)
  where
    hasText (Node { _node_hyperdata = h }) = hasText h



-- | TODO putelsewhere
-- | Upgrade function
-- Suppose all documents are English (this is the case actually)
indexAllDocumentsWithPosTag :: FlowCmdM env err m
                            => m ()
indexAllDocumentsWithPosTag = do
  rootId    <- getRootId (UserName userMaster)
  corpusIds <- findNodesId rootId [NodeCorpus]
  docs      <- List.concat <$> mapM getDocumentsWithParentId corpusIds
  _ <- mapM extractInsert (splitEvery 1000 docs)
  pure ()

extractInsert :: FlowCmdM env err m
              => [Node HyperdataDocument] -> m ()
extractInsert docs = do
  let documentsWithId = map (\doc -> Indexed (doc ^. node_id) doc) docs
  let lang = EN
  ncs <- view $ nlpServerGet lang
  mapNgramsDocs' <- mapNodeIdNgrams
                <$> documentIdWithNgrams
                    (extractNgramsT ncs $ withLang (Multi lang) documentsWithId)
                    documentsWithId
  _ <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'
  pure ()



-- | Re-index documents of a corpus with ngrams in the list
reIndexWith :: ( HasNodeStory env err m )
            => CorpusId
            -> ListId
            -> NgramsType
            -> Set ListType
            -> m ()
reIndexWith cId lId nt lts = do
  -- printDebug "(cId,lId,nt,lts)" (cId, lId, nt, lts)
  corpus_node <- getNodeWith cId (Proxy @ HyperdataCorpus)
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
  _ <- mapM (saveDocNgramsWith lId) ngramsByDoc
  pure ()


docNgrams :: Lang
          -> NgramsType
          -> [NT.NgramsTerm]
          -> Gargantext.Database.Admin.Types.Node.Context HyperdataDocument
          -> [((MatchedText, TermsCount),
                Map NgramsType (Map NodeId Int))]
docNgrams lang nt ts doc =
  List.zip
  (termsInText lang (buildPatternsWith lang ts)
    $ T.unlines $ catMaybes
    [ doc ^. context_hyperdata . hd_title
    , doc ^. context_hyperdata . hd_abstract
    ]
  )
  (List.cycle [Map.fromList $ [(nt, Map.singleton (doc ^. context_id) 1 )]])



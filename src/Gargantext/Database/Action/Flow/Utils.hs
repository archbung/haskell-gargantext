{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.Database.Action.Flow.Utils
  ( docNgrams
  , documentIdWithNgrams
  , insertDocNgrams
  , insertDocs
  , mapNodeIdNgrams )
where

import Control.Lens ((^.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict qualified as DM
import Data.Text qualified as T
import Gargantext.API.Ngrams.Types qualified as NT
import Gargantext.Core (Lang, toDBid)
import Gargantext.Core.Flow.Types (UniqId, uniqId)
import Gargantext.Core.Text.Ngrams ( Ngrams, NgramsType )
import Gargantext.Core.Text.Terms.WithList (MatchedText, buildPatternsWith, termsInText)
import Gargantext.Core.Types (TermsCount)
import Gargantext.Core.Utils (addTuples)
import Gargantext.Data.HashMap.Strict.Utils qualified as HashMap
import Gargantext.Database.Action.Flow.Types (DocumentIdWithNgrams(..), FlowInsertDB)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument, hd_abstract, hd_title )
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd, DbCmd')
import Gargantext.Database.Query.Table.ContextNodeNgrams
import Gargantext.Database.Query.Table.Node.Document.Add qualified as Doc (add)
import Gargantext.Database.Query.Table.Node.Document.Insert (ReturnId, addUniqId, insertDb, reId, reInserted, reUniqId)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Schema.Context (context_hyperdata, context_id)
import Gargantext.Database.Schema.Ngrams (NgramsId, NgramsTypeId(..))
import Gargantext.Database.Types ( Indexed(..), index )
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)


insertDocNgrams :: ListId
                -> HashMap (Indexed NgramsId Ngrams) (Map NgramsType (Map DocId (Int, TermsCount)))
                -> DBCmd err Int
insertDocNgrams lId m = do
  -- printDebug "[insertDocNgrams] ns" ns
  insertContextNodeNgrams ns
  where
    ns = [ ContextNodeNgrams (nodeId2ContextId docId)
                             lId (ng^.index)
                             (NgramsTypeId $ toDBid t)
                             (fromIntegral i)
                             cnt
         | (ng, t2n2i)       <- HashMap.toList m
         , (t,  n2i)         <- DM.toList t2n2i
         , (docId, (i, cnt)) <- DM.toList n2i
         ]

-- [(NodeId, {Ngrams: ({NgramsType: Int}, TermsCount)})]
-- {Ngrams: {NgramsType: {NodeId: (Int, TermsCount)}}}





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
  (List.cycle [DM.fromList $ [(nt, DM.singleton (doc ^. context_id) 1 )]])


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
mapNodeIdNgrams = HashMap.unionsWith (DM.unionWith (DM.unionWith addTuples)) . fmap f
  where
    -- | NOTE We are somehow multiplying 'TermsCount' here: If the
    -- same ngrams term has different ngrams types, the 'TermsCount'
    -- for it (which is the number of times the terms appears in a
    -- document) is copied over to all its types.
    f :: DocumentIdWithNgrams a b
      -> HashMap.HashMap b (Map NgramsType (Map NodeId (Int, TermsCount)))
    f d = fmap (\(ngramsTypeMap, cnt) -> fmap (\i -> DM.singleton nId (i, cnt)) ngramsTypeMap) $ documentNgrams d
      where
        nId = _index $ documentWithId d

        
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
    newIds' = map (nodeId2ContextId . reId) newIds
    documentsWithId = mergeData (toInserted newIds) (DM.fromList $ map viewUniqId' docs)
  _ <- Doc.add cId newIds'
  pure (newIds', map (first nodeId2ContextId) documentsWithId)


------------------------------------------------------------------------
viewUniqId' :: UniqId a
            => a
            -> (Hash, a)
viewUniqId' d = maybe err (\h -> (h,d)) (d ^. uniqId)
      where
        err = panicTrace "[ERROR] Database.Flow.toInsert"


mergeData :: Map Hash ReturnId
          -> Map Hash a
          -> [Indexed NodeId a]
mergeData rs = catMaybes . map toDocumentWithId . DM.toList
  where
    toDocumentWithId (sha,hpd) =
      Indexed <$> fmap reId (DM.lookup sha rs)
              <*> Just hpd




toInserted :: [ReturnId]
           -> Map Hash ReturnId
toInserted =
  DM.fromList . map    (\r -> (reUniqId r, r)     )
              . filter (\r -> reInserted r == True)



-- Apparently unused functions


-- | TODO putelsewhere
-- | Upgrade function
-- Suppose all documents are English (this is the case actually)
-- indexAllDocumentsWithPosTag :: ( HasNodeStory env err m
--                                , HasNLPServer env )
--                             => m ()
-- indexAllDocumentsWithPosTag = do
--   rootId    <- getRootId (UserName userMaster)
--   corpusIds <- findNodesId rootId [NodeCorpus]
--   docs      <- List.concat <$> mapM getDocumentsWithParentId corpusIds
--   _ <- mapM extractInsert (splitEvery 1000 docs)
--   pure ()

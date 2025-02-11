{-|
Module      : Gargantext.Core.Text.Ngrams.Lists
Description : Tools to build lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.List
  where

import Control.Lens ( view, over ) -- ((^.), view, over, set, (_1), (_2))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Extra (both)
import Gargantext.API.Ngrams.Types (NgramsElement, NgramsTerm(..))
import Gargantext.Core.NLP (HasNLPServer)
import Gargantext.Core.NodeStory.Types ( HasNodeStory )
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Group ( toGroupedTree, setScoresWithMap )
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Social ( FlowSocialListWith, flowSocialList )
import Gargantext.Core.Text.List.Social.Prelude ( FlowListScores, FlowCont(FlowCont), flc_scores )
import Gargantext.Core.Text.Metrics (scored', Scored(..), scored_speExc, scored_genInc, normalizeGlobal, normalizeLocal, scored_terms)
import Gargantext.Core.Text.Ngrams (NgramsType(..), Ngrams(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main ( ListType(..) )
import Gargantext.Data.HashMap.Strict.Utils qualified as HashMap
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsUser, getContextsByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf_withSample)
import Gargantext.Database.Admin.Types.Node ( MasterCorpusId, UserCorpusId, ContextId )
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.NgramsPostag (selectLems)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Schema.Ngrams (text2ngrams)
import Gargantext.Prelude

{-
-- TODO maybe useful for later
isStopTerm :: StopSize -> Text -> Bool
isStopTerm (StopSize n) x = Text.length x < n || any isStopChar (Text.unpack x)
  where
    isStopChar c = not (c `elem` ("- /()%" :: [Char]) || Char.isAlpha c)
-}


-- | TODO improve grouping functions of Authors, Sources, Institutes..
buildNgramsLists :: ( HasNodeStory env err m
                    , HasNLPServer env
                    , HasTreeError err
                    , HasNodeError err
                    )
                 => User
                 -> UserCorpusId
                 -> MasterCorpusId
                 -> Maybe FlowSocialListWith
                 -> GroupParams
                 -> m (Map NgramsType [NgramsElement])
buildNgramsLists user uCid mCid mfslw gp = do
  ngTerms     <- buildNgramsTermsList user uCid mCid mfslw gp (NgramsTerms, MapListSize 350)
  othersTerms <- mapM (buildNgramsOthersList user uCid mfslw GroupIdentity)
                      [ (Authors   , MapListSize 9, MaxListSize 1000)
                      , (Sources   , MapListSize 9, MaxListSize 1000)
                      , (Institutes, MapListSize 9, MaxListSize 1000)
                      ]

  pure $ Map.unions $ [ngTerms] <> othersTerms


newtype MapListSize = MapListSize { unMapListSize :: Int }
newtype MaxListSize = MaxListSize { unMaxListSize :: Int }

buildNgramsOthersList :: ( HasNodeError err
                         , HasNLPServer env
                         , HasNodeStory env err m
                         , HasTreeError err
                         )
                      => User
                      -> UserCorpusId
                      -> Maybe FlowSocialListWith
                      -> GroupParams
                      -> (NgramsType, MapListSize, MaxListSize)
                      -> m (Map NgramsType [NgramsElement])
buildNgramsOthersList user uCid mfslw _groupParams (nt, MapListSize mapListSize, MaxListSize maxListSize) = do
  allTerms  :: HashMap NgramsTerm (Set ContextId) <- getContextsByNgramsUser uCid nt

  -- PrivateFirst for first developments since Public NodeMode is not implemented yet
  socialLists :: FlowCont NgramsTerm FlowListScores
    <- flowSocialList mfslw user nt ( FlowCont HashMap.empty
                                                      $ HashMap.fromList
                                                      $ List.zip (HashMap.keys allTerms)
                                                                 (repeat mempty)
                                    )
  let
    groupedWithList = toGroupedTree {- groupParams -} socialLists allTerms

    (stopTerms, tailTerms) = HashMap.partition ((== Just StopTerm) . viewListType)
                           $ view flc_scores groupedWithList

    (mapTerms, tailTerms') = HashMap.partition ((== Just MapTerm)  . viewListType) tailTerms

    listSize = mapListSize - List.length mapTerms
    (mapTerms', candiTerms) = both HashMap.fromList
                            $ List.splitAt listSize
                            $ List.take maxListSize
                            $ List.sortOn (Down . viewScore . snd)
                            $ HashMap.toList tailTerms'
    
  pure $ Map.fromList [( nt, List.take maxListSize $ toNgramsElement stopTerms
                          <> toNgramsElement mapTerms
                          <> toNgramsElement (setListType (Just MapTerm      ) mapTerms')
                          <> toNgramsElement (setListType (Just CandidateTerm) candiTerms)
                          )]


-- | https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/169#note_10049
-- Stemming can be useful if you do not have any context: ok for full text search then.
-- 
-- In document, we have context so we can add grammar and linguistics
-- rules to be more precise than the stemmatization, that is why the
-- lemmatization is used here to group. Basically it will avoid
-- grouping homonyms in list. In search usually you add more context
-- to "control" the stemmatization approximation.
getGroupParams :: ( HasNodeError err
                  , HasTreeError err
                  )
               => GroupParams -> HashSet Ngrams -> DBCmd err GroupParams
getGroupParams gp@(GroupWithPosTag { .. }) ng = do
  !hashMap <- HashMap.fromList <$> selectLems _gwl_lang _gwl_nlp_config (HashSet.toList ng)
  -- printDebug "hashMap" hashMap
  pure $ over gwl_map (<> hashMap) gp
getGroupParams gp _ = pure gp


-- TODO use ListIds
buildNgramsTermsList :: ( HasNodeError err
                        , HasNLPServer env
                        , HasNodeStory env err m
                        , HasTreeError err
                        )
                     => User
                     -> UserCorpusId
                     -> MasterCorpusId
                     -> Maybe FlowSocialListWith
                     -> GroupParams
                     -> (NgramsType, MapListSize)
                     -> m (Map NgramsType [NgramsElement])
buildNgramsTermsList user uCid mCid mfslw groupParams (nt, MapListSize mapListSize) = do

-- Filter 0 With Double
-- Computing global speGen score
  -- printDebug "[buildNgramsTermsList: Sample List] / start" nt
  !(allTerms :: HashMap NgramsTerm Double) <- getTficf_withSample uCid mCid nt

  -- printDebug "[buildNgramsTermsList: Sample List / end]" (nt, HashMap.size allTerms)

  -- printDebug "[buildNgramsTermsList: Flow Social List / start]" nt

  -- PrivateFirst for first developments since Public NodeMode is not implemented yet
  !(socialLists :: FlowCont NgramsTerm FlowListScores)
    <- flowSocialList mfslw user nt ( FlowCont HashMap.empty
                                                      $ HashMap.fromList
                                                      $ List.zip (HashMap.keys allTerms)
                                                                 (repeat mempty)
                                    )
  -- printDebug "[buildNgramsTermsList: Flow Social List / end]" nt

  let !ngramsKeys = HashSet.fromList
                  $ List.take mapListSize
                  $ HashSet.toList
                  $ HashMap.keysSet allTerms

  -- printDebug "[buildNgramsTermsList: ngramsKeys]" (HashSet.size ngramsKeys)

  !groupParams' <- getGroupParams groupParams (HashSet.map (text2ngrams . unNgramsTerm) ngramsKeys)

  let
    !socialLists_Stemmed = addScoreStem groupParams' ngramsKeys socialLists
    !groupedWithList = toGroupedTree socialLists_Stemmed allTerms
    !(stopTerms, candidateTerms) = HashMap.partition ((== Just StopTerm) . viewListType)
                                 $ HashMap.filter (\g -> view gts'_score g > 1)
                                 $ view flc_scores groupedWithList

    !(groupedMono, groupedMult)  = HashMap.partitionWithKey (\(NgramsTerm t) _v -> size t < 2) candidateTerms

  -- void $ panicTrace $ "groupedWithList: " <> show groupedWithList

  -- printDebug "[buildNgramsTermsList] socialLists" socialLists
  -- printDebug "[buildNgramsTermsList] socialLists with scores" socialLists_Stemmed
  -- printDebug "[buildNgramsTermsList] groupedWithList" groupedWithList
  -- printDebug "[buildNgramsTermsList] stopTerms" stopTerms

  -- splitting monterms and multiterms to take proportional candidates
    -- use % of list if to big, or Int if too small
  let
    !listSizeGlobal = 2000 :: Double
    !monoSize = 0.4  :: Double
    !multSize = 1 - monoSize

    splitAt' n' ns = both (HashMap.fromListWith (<>))
                      $ List.splitAt (round $ n' * listSizeGlobal)
                      $ List.sortOn (viewScore . snd)
                      $ HashMap.toList ns

    !(groupedMonoHead, _groupedMonoTail) = splitAt' monoSize groupedMono
    !(groupedMultHead, groupedMultTail)  = splitAt' multSize groupedMult

-------------------------
-- Filter 1 With Set NodeId and SpeGen
    !selectedTerms = Set.toList $ hasTerms (groupedMonoHead <> groupedMultHead)

  -- printDebug "[buildNgramsTermsList: selectedTerms]" selectedTerms

 -- TODO remove (and remove HasNodeError instance)
  !userListId    <- defaultList uCid
  !masterListId  <- defaultList mCid

  !mapTextDocIds <- getContextsByNgramsOnlyUser uCid
                                            [userListId, masterListId]
                                            nt
                                            selectedTerms


  -- printDebug "[buildNgramsTermsList: mapTextDocIds]" mapTextDocIds

  let
    groupedTreeScores_SetNodeId :: HashMap NgramsTerm (GroupedTreeScores (Set ContextId))
    !groupedTreeScores_SetNodeId = HashMap.filter (\g -> Set.size (view gts'_score g) > 1) -- removing hapax
                                $ setScoresWithMap mapTextDocIds (groupedMonoHead <> groupedMultHead)


  -- printDebug "[buildNgramsTermsList: groupedTreeScores_SetNodeId]" groupedTreeScores_SetNodeId

  -- Coocurrences computation
  --, t1 >= t2 -- permute byAxis diag  -- since matrix symmetric
  let !mapCooc = HashMap.filter (>1) -- removing cooc of 1
              $ HashMap.fromList [ ((t1, t2), Set.size $ Set.intersection s1 s2)
                           | (t1, s1) <- mapStemNodeIds
                           , (t2, s2) <- mapStemNodeIds
                           ]
          where
            mapStemNodeIds = HashMap.toList
                           $ HashMap.map viewScores
                           $ groupedTreeScores_SetNodeId
  let
    -- computing scores
    mapScores f = HashMap.fromList
                $ map (\g -> (view scored_terms g, f g))
                $ normalizeGlobal
                $ map normalizeLocal
                $ scored'
                $ Map.fromList -- TODO remove this
                $ HashMap.toList mapCooc

  let
    groupedTreeScores_SpeGen :: HashMap NgramsTerm (GroupedTreeScores (Scored NgramsTerm))
    !groupedTreeScores_SpeGen = setScoresWithMap (mapScores identity) groupedTreeScores_SetNodeId

  let
    -- sort / partition / split
    -- filter mono/multi again
    !(monoScored, multScored) = HashMap.partitionWithKey (\(NgramsTerm t) _v -> size t < 2) groupedTreeScores_SpeGen

      -- filter with max score
    partitionWithMaxScore = HashMap.partition (\g -> view scored_genInc (view gts'_score g)
                                                   > view scored_speExc (view gts'_score g)
                                              )

    !(monoScoredIncl, monoScoredExcl) = partitionWithMaxScore monoScored
    !(multScoredIncl, multScoredExcl) = partitionWithMaxScore multScored

  -- splitAt
  let
    -- use % of list if to big, or Int if to small
    !mapSize = 1000 :: Double
    !canSize = mapSize * 2 :: Double

    !inclSize = 0.4  :: Double
    !exclSize = 1 - inclSize

    splitAt'' max' n' = both HashMap.fromList . List.splitAt (round $ n' * max')
    sortOn'   f       = List.sortOn (Down . view (gts'_score . f) . snd) . HashMap.toList

    monoInc_size n = splitAt'' n $ monoSize * inclSize / 2
    multExc_size n = splitAt'' n $ multSize * exclSize / 2


    !(mapMonoScoredInclHead, monoScoredInclTail) = monoInc_size mapSize $ sortOn' scored_genInc monoScoredIncl
    !(mapMonoScoredExclHead, monoScoredExclTail) = monoInc_size mapSize $ sortOn' scored_speExc monoScoredExcl

    !(mapMultScoredInclHead, multScoredInclTail) = multExc_size mapSize $ sortOn' scored_genInc multScoredIncl
    !(mapMultScoredExclHead, multScoredExclTail) = multExc_size mapSize $ sortOn' scored_speExc multScoredExcl


    !(canMonoScoredIncHead , _) = monoInc_size canSize $ sortOn' scored_genInc monoScoredInclTail
    !(canMonoScoredExclHead, _) = monoInc_size canSize $ sortOn' scored_speExc monoScoredExclTail

    !(canMulScoredInclHead, _)  = multExc_size canSize $ sortOn' scored_genInc multScoredInclTail
    !(canMultScoredExclHead, _) = multExc_size canSize $ sortOn' scored_speExc multScoredExclTail

------------------------------------------------------------
    -- Final Step building the Typed list
    -- Candidates Terms need to be filtered
  let
    !maps = setListType (Just MapTerm)
        $  mapMonoScoredInclHead
        <> mapMonoScoredExclHead
        <> mapMultScoredInclHead
        <> mapMultScoredExclHead

    -- An original way to filter to start with
    !cands = setListType (Just CandidateTerm)
          $ canMonoScoredIncHead
          <> canMonoScoredExclHead
          <> canMulScoredInclHead
          <> canMultScoredExclHead

  -- TODO count it too
    !cands' = setListType (Just CandidateTerm)
          {-\$  groupedMonoTail
          <>-} groupedMultTail

    -- Quick FIX
    !candNgramsElement = List.take 1000
                      $ toNgramsElement cands <> toNgramsElement cands'

    !result = Map.unionsWith (<>)
       [ Map.fromList [( nt, toNgramsElement maps
                          <> toNgramsElement stopTerms
                          <> candNgramsElement
                      )]
       ]

  pure result

{-|
Module      : Gargantext.Database.Metrics.TFICF
Description : Ngrams by Node user and master
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Database.Action.Metrics.TFICF
  where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Gargantext.API.Ngrams.Types
import Gargantext.Core
import Gargantext.Core.Text.Metrics.TFICF
import Gargantext.Core.Text.Ngrams (NgramsType(..))
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsUser, {-getOccByNgramsOnlyFast,-} getOccByNgramsOnlyFast_withSample)
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.NodeContext (selectCountDocs)
import Gargantext.Prelude

{-
getTficf :: HasDBid NodeType 
         => UserCorpusId
         -> MasterCorpusId
         -> NgramsType
         -> Cmd err (HashMap NgramsTerm Double)
getTficf cId mId nt = do
  mapTextDoubleLocal <- HM.filter (> 1)
     <$> HM.map (fromIntegral . Set.size)
     <$> getContextsByNgramsUser cId nt

  mapTextDoubleGlobal <- HM.map fromIntegral
                     <$> getOccByNgramsOnlyFast mId nt (HM.keys mapTextDoubleLocal)

  countLocal  <- selectCountDocs cId
  countGlobal <- selectCountDocs mId

  printDebug "getTficf" (mapTextDoubleLocal, mapTextDoubleGlobal, countLocal, countGlobal)

  pure $ HM.mapWithKey (\t n ->
      tficf (TficfInfra (Count n                                               )
                        (Total $ fromIntegral countLocal))
            (TficfSupra (Count $ fromMaybe 0 $ HM.lookup t mapTextDoubleGlobal)
                        (Total $ fromIntegral countGlobal))
    ) mapTextDoubleLocal
-}

getTficf_withSample :: HasDBid NodeType
                    => UserCorpusId
                    -> MasterCorpusId
                    -> NgramsType
                    -> DBCmd err (HashMap NgramsTerm Double)
getTficf_withSample cId mId nt = do
  mapTextDoubleLocal <- HM.filter (> 1)
     <$> HM.map (fromIntegral . Set.size)
     <$> getContextsByNgramsUser cId nt

  countLocal  <- selectCountDocs cId
  let countGlobal = countLocal
      -- * 10

  mapTextDoubleGlobal <- HM.map fromIntegral
                     <$> getOccByNgramsOnlyFast_withSample mId countGlobal nt
                            (HM.keys mapTextDoubleLocal)

  --printDebug "getTficf_withSample" (mapTextDoubleLocal, mapTextDoubleGlobal, countLocal, countGlobal)
  pure $ HM.mapWithKey (\t n ->
      tficf (TficfInfra (Count n                                               )
                        (Total $ fromIntegral countLocal))
            (TficfSupra (Count $ fromMaybe 0 $ HM.lookup t mapTextDoubleGlobal)
                        (Total $ fromIntegral countGlobal))
    ) mapTextDoubleLocal


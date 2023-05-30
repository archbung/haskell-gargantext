{-|
Module      : Gargantext.Core.Text.Corpus.API.Arxiv
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}
{-# LANGUAGE ViewPatterns #-}

module Gargantext.Core.Text.Corpus.API.Arxiv
    ( get
    -- * Internals for testing
    , convertQuery
    ) where

import Conduit
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as Text

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Core.Text.Corpus.Query as Corpus
import Gargantext.Core.Types (Term(..))

import qualified Arxiv as Arxiv
import qualified Network.Api.Arxiv as Ax


-- | Converts a Gargantext's generic boolean query into an Arxiv Query.
convertQuery :: Corpus.Query -> Ax.Query
convertQuery q = mkQuery (interpretQuery q transformAST)
  where
    mkQuery :: Maybe Ax.Expression -> Ax.Query
    mkQuery mb_exp = Ax.Query { Ax.qExp = mb_exp
                              , Ax.qIds = []
                              , Ax.qStart = 0
                              , Ax.qItems = Arxiv.batchSize }

    -- Converts a 'BoolExpr' with 'Term's on the leaves into an Arxiv's expression.
    -- It yields 'Nothing' if the AST cannot be converted into a meaningful expression.
    transformAST :: BoolExpr Term -> Maybe Ax.Expression
    transformAST ast = case ast of
      BAnd sub (BConst (Negative term))
        -- The second term become positive, so that it can be translated.
        -> Ax.AndNot <$> (transformAST sub) <*> transformAST (BConst (Positive term))
      BAnd term1 (BNot term2)
        -> Ax.AndNot <$> transformAST term1 <*> transformAST term2
      BAnd sub1 sub2
        -> Ax.And <$> transformAST sub1 <*> transformAST sub2
      BOr sub1 sub2
        -> Ax.Or <$> transformAST sub1 <*> transformAST sub2
      BNot (BConst (Negative term))
        -> transformAST (BConst (Positive term)) -- double negation
      -- We can handle negatives via `ANDNOT` with itself.
      BNot sub
        -> Ax.AndNot <$> transformAST sub <*> transformAST sub
      -- BTrue cannot happen is the query parser doesn't support parsing 'TRUE' alone.
      BTrue
        -> Nothing
      -- BTrue cannot happen is the query parser doesn't support parsing 'FALSE' alone.
      BFalse
        -> Nothing
      BConst (Positive (Term term))
        -> Just $ Ax.Exp $ Ax.Abs [unpack term]
      -- We can handle negatives via `ANDNOT` with itself.
      BConst (Negative (Term term))
        -> Just $ Ax.AndNot (Ax.Exp $ Ax.Abs [unpack term]) (Ax.Exp $ Ax.Abs [unpack term])

-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Lang
    -> Corpus.Query
    -> Maybe Arxiv.Limit
    -> IO (Maybe Integer, ConduitT () HyperdataDocument IO ())
get la (convertQuery -> query) limit = do
  (cnt, resC) <- case limit of
    Nothing  -> Arxiv.searchAxv' query
    (Just l) -> do (cnt, res) <- Arxiv.searchAxv' query
                   pure (cnt, res .| takeC l)
  pure $ (Just $ fromIntegral cnt, resC .| mapC (toDoc la))

toDoc :: Lang -> Arxiv.Result -> HyperdataDocument
toDoc l (Arxiv.Result { abstract
                      , authors = aus
                      --, categories
                      , doi
                      , id
                      , journal
                      --, primaryCategory
                      , publication_date
                      , title
                      --, total
                      , url
                      , year }
          ) = HyperdataDocument { _hd_bdd = Just "Arxiv"
                                , _hd_doi = Just $ Text.pack doi
                                , _hd_url = Just $ Text.pack url
                                , _hd_uniqId = Just $ Text.pack id
                                , _hd_uniqIdBdd = Nothing
                                , _hd_page = Nothing
                                , _hd_title = Just $ Text.pack title
                                , _hd_authors = authors aus
                                , _hd_institutes = institutes aus
                                , _hd_source = Just $ Text.pack journal
                                , _hd_abstract = Just $ Text.pack abstract
                                , _hd_publication_date = Just $ Text.pack publication_date
                                , _hd_publication_year = fromIntegral <$> year
                                , _hd_publication_month = Nothing  -- TODO parse publication_date
                                , _hd_publication_day = Nothing
                                , _hd_publication_hour = Nothing
                                , _hd_publication_minute = Nothing
                                , _hd_publication_second = Nothing
                                , _hd_language_iso2 = Just $ (Text.pack . show) l }
      where
        authors :: [Ax.Author] -> Maybe Text
        authors [] = Nothing
        authors aus' = Just $ (Text.intercalate ", ")
                            $ map Text.pack
                            $ map Ax.auName aus'

        institutes :: [Ax.Author] -> Maybe Text
        institutes [] = Nothing
        institutes aus' = Just $ (Text.intercalate ", ")
                               $ (map (Text.replace ", " " - "))
                               $ map Text.pack
                               $ map Ax.auFil aus'

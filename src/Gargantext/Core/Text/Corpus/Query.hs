{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Gargantext.Core.Text.Corpus.Query (
    Query -- * opaque
  , RawQuery(..)
  , Limit(..)
  , getQuery
  , parseQuery
  , renderQuery
  , ExternalAPIs(..)

  -- * Useful for testing
  , unsafeMkQuery
  ) where

import           Data.Bifunctor
import           Data.String
import           Gargantext.API.Admin.Orchestrator.Types
import           Gargantext.Core.Types
import           Prelude
import qualified Data.Aeson                               as Aeson
import qualified Data.BoolExpr                            as BoolExpr
import qualified Data.BoolExpr.Parser                     as BoolExpr
import qualified Data.BoolExpr.Printer                    as BoolExpr
import qualified Data.Swagger                             as Swagger
import qualified Data.Text                                as T
import qualified Servant.API                              as Servant
import qualified Text.Parsec                              as P

-- | A raw query, as typed by the user from the frontend.
newtype RawQuery = RawQuery { getRawQuery :: T.Text }
  deriving newtype ( Show, Eq, IsString
                   , Servant.FromHttpApiData, Servant.ToHttpApiData
                   , Aeson.FromJSON, Aeson.ToJSON
                   , Swagger.ToParamSchema, Swagger.ToSchema)

-- | A limit to the number of results we want to retrieve.
newtype Limit = Limit { getLimit :: Int }
  deriving newtype ( Show, Eq, Num
                   , Servant.FromHttpApiData, Servant.ToHttpApiData
                   , Aeson.FromJSON, Aeson.ToJSON
                   , Swagger.ToParamSchema, Swagger.ToSchema)

-- | An opaque wrapper around a 'Query' type which can be parsed from a boolean
-- expression like (a AND b) OR c, and which can be interpreted in many ways
-- according to the particular service we are targeting.
newtype Query = Query { getQuery :: (BoolExpr.CNF Term) }
  deriving Show

unsafeMkQuery :: BoolExpr.BoolExpr Term -> Query
unsafeMkQuery = Query . BoolExpr.boolTreeToCNF

-- | Parses an input 'Text' into a 'Query', reporting an error if it fails.
parseQuery :: RawQuery -> Either String Query
parseQuery (RawQuery txt) = bimap show (Query . BoolExpr.boolTreeToCNF) $
  P.runParser (BoolExpr.parseBoolExpr (Term . T.pack <$> BoolExpr.identifier)) () "Corpus.Query" (T.unpack txt)

renderQuery :: Query -> RawQuery
renderQuery (Query cnf) = RawQuery . T.pack $ BoolExpr.boolExprPrinter (showsPrec 0) (BoolExpr.fromCNF cnf) ""

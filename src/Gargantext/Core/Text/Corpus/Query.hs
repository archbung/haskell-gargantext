{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Gargantext.Core.Text.Corpus.Query (
    Query -- * opaque
  , RawQuery(..)
  , Limit(..)
  , getQuery
  , parseQuery
  , renderQuery
  , interpretQuery
  , ExternalAPIs(..)
  , module BoolExpr

  -- * Useful for testing
  , unsafeMkQuery
  ) where

import           Data.Bifunctor
import           Data.String
import           Gargantext.API.Admin.Orchestrator.Types
import           Gargantext.Core.Types
import           Prelude
import           Text.ParserCombinators.Parsec
import qualified Data.Aeson                               as Aeson
import           Data.BoolExpr                            as BoolExpr
import           Data.BoolExpr.Parser                     as BoolExpr
import           Data.BoolExpr.Printer                    as BoolExpr
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

interpretQuery :: Query -> (BoolExpr.BoolExpr Term -> ast) -> ast
interpretQuery (Query q) transform = transform (BoolExpr.fromCNF q)

unsafeMkQuery :: BoolExpr.BoolExpr Term -> Query
unsafeMkQuery = Query . BoolExpr.boolTreeToCNF

termToken :: CharParser st Term
termToken = Term <$> (try (T.pack <$> BoolExpr.identifier) <|> (between dubQuote dubQuote multipleTerms))
  where
    dubQuote      = BoolExpr.symbol "\""
    multipleTerms = T.intercalate " " . map T.pack <$> sepBy BoolExpr.identifier BoolExpr.whiteSpace

-- | Parses an input 'Text' into a 'Query', reporting an error if it fails.
parseQuery :: RawQuery -> Either String Query
parseQuery (RawQuery txt) = bimap show (Query . BoolExpr.boolTreeToCNF) $
  P.runParser (BoolExpr.parseBoolExpr termToken) () "Corpus.Query" (T.unpack txt)

renderQuery :: Query -> RawQuery
renderQuery (Query cnf) = RawQuery . T.pack $ BoolExpr.boolExprPrinter (showsPrec 0) (BoolExpr.fromCNF cnf) ""

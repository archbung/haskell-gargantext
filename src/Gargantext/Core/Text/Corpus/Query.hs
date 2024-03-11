{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
module Gargantext.Core.Text.Corpus.Query (
    Query -- * opaque
  , RawQuery(..)
  , Limit(..)
  , QueryTerm(..)
  , getQuery
  , parseQuery
  , mapQuery
  , renderQuery
  , renderQueryTerm
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
import           Test.QuickCheck
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

instance Arbitrary RawQuery where
  arbitrary = RawQuery <$> arbitrary

-- | A limit to the number of results we want to retrieve.
newtype Limit = Limit { getLimit :: Int }
  deriving newtype ( Show, Eq, Num
                   , Servant.FromHttpApiData, Servant.ToHttpApiData
                   , Aeson.FromJSON, Aeson.ToJSON
                   , Swagger.ToParamSchema, Swagger.ToSchema)

-- | A /query/ term, i.e. a node of the query expression tree which can be
-- either a Gargantext 'Term' (i.e. just a textual value) or something else,
-- like a partial match (i.e. the user is asking to perform a search that would
-- match only a suffix of a word).
data QueryTerm
  = QT_exact_match Term
  | QT_partial_match Term
  deriving (Show, Eq, Ord)

instance IsString QueryTerm where
  fromString input = case P.runParser queryTermToken () "Corpus.Query.fromString" input of
    Left _     -> QT_exact_match (Term $ T.pack input)
    Right [qt] -> qt
    Right _    -> QT_exact_match (Term $ T.pack input)

renderQueryTerm :: QueryTerm -> T.Text
renderQueryTerm (QT_exact_match   (Term t)) = t
renderQueryTerm (QT_partial_match (Term t)) = t

-- | An opaque wrapper around a 'Query' type which can be parsed from a boolean
-- expression like (a AND b) OR c, and which can be interpreted in many ways
-- according to the particular service we are targeting.
newtype Query = Query { getQuery :: (BoolExpr.CNF [QueryTerm]) }
  deriving Show

interpretQuery :: Query -> (BoolExpr.BoolExpr [QueryTerm] -> ast) -> ast
interpretQuery (Query q) transform = transform . simplify . BoolExpr.fromCNF $ q

simplify :: BoolExpr.BoolExpr a -> BoolExpr.BoolExpr a
simplify expr = case expr of
  BAnd sub BTrue    -> simplify sub
  BAnd BTrue sub    -> simplify sub
  BAnd BFalse _     -> BFalse
  BAnd _ BFalse     -> BFalse
  BAnd sub1 sub2    -> BAnd (simplify sub1) (simplify sub2)
  BOr _ BTrue       -> BTrue
  BOr BTrue _       -> BTrue
  BOr sub BFalse    -> simplify sub
  BOr BFalse sub    -> simplify sub
  BOr sub1 sub2     -> BOr (simplify sub1) (simplify sub2)
  BNot BTrue        -> BFalse
  BNot BFalse       -> BTrue
  BNot (BNot sub)   -> simplify sub
  BNot sub          -> BNot (simplify sub)
  BTrue             -> BTrue
  BFalse            -> BFalse
  BConst signed     -> BConst signed

unsafeMkQuery :: BoolExpr.BoolExpr [QueryTerm] -> Query
unsafeMkQuery = Query . BoolExpr.boolTreeToCNF

queryTermToken :: CharParser st [QueryTerm]
queryTermToken = do
 map mkQueryTerm <$> termToken
 where
   mkQueryTerm :: Term -> QueryTerm
   mkQueryTerm (Term (T.unpack -> t)) =
     case t of
     '"' : '~' : rest
       -> QT_partial_match (Term $ T.pack $ '"' : rest)
     '~' : rest
       -> QT_partial_match (Term $ T.pack $ '"' : rest)
     _
       -> QT_exact_match (Term $ T.pack t)

termToken :: CharParser st [Term]
termToken = (try ((:[]) . Term . T.pack <$> BoolExpr.identifier) <|> (between dubQuote dubQuote multipleTerms))
  where
    dubQuote      = BoolExpr.symbol "\""
    multipleTerms = map (Term . T.pack) <$> sepBy BoolExpr.identifier BoolExpr.whiteSpace

-- | Parses an input 'Text' into a 'Query', reporting an error if it fails.
parseQuery :: RawQuery -> Either String Query
parseQuery (RawQuery txt) = bimap show (Query . BoolExpr.boolTreeToCNF) $
  P.runParser (BoolExpr.parseBoolExpr queryTermToken) () "Corpus.Query" (T.unpack txt)

renderQuery :: Query -> RawQuery
renderQuery (Query cnf) = RawQuery . T.pack $ BoolExpr.boolExprPrinter (showsPrec 0) (BoolExpr.fromCNF cnf) ""

mapQuery :: (QueryTerm -> QueryTerm) -> Query -> Query
mapQuery f = Query . fmap (map f) . getQuery

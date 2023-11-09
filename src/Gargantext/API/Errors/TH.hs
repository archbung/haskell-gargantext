{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargantext.API.Errors.TH (
    deriveHttpStatusCode
  , deriveIsFrontendErrorData
  ) where

import           Prelude

import           Gargantext.API.Errors.Types.Backend
import           Network.HTTP.Types
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import qualified Language.Haskell.TH            as TH
import qualified Network.HTTP.Types as HTTP

-- | A static map of the HTTP status code we support.
supported_http_status_map :: Map.Map T.Text (TH.Q TH.Exp)
supported_http_status_map = Map.fromList
  [ ("200", TH.varE 'status200)
  , ("400", TH.varE 'status400)
  , ("403", TH.varE 'status403)
  , ("404", TH.varE 'status404)
  , ("500", TH.varE 'status500)
  ]


deriveHttpStatusCode :: TH.Name -> TH.Q [TH.Dec]
deriveHttpStatusCode appliedType = do
  info <- TH.reify appliedType
  case info of
    TH.TyConI (TH.DataD _ _ _ _ ctors _)
      -> case extract_names ctors of
           Left ctor   -> error $ "Only enum-like constructors supported: " ++ show ctor
           Right names -> case parse_error_codes names of
             Left n -> error $ "Couldn't extract error code from : " ++ TH.nameBase n
                             ++ ". Make sure it's in the form XX_<validHttpStatusCode>__<textual_diagnostic>"
             Right codes -> do
               let static_matches = flip map codes $ \(n, stE, _txt) ->
                     TH.match (TH.conP n [])
                              (TH.normalB [| $(stE) |])
                              []

               [d| backendErrorTypeToErrStatus :: BackendErrorCode -> HTTP.Status
                   backendErrorTypeToErrStatus = $(TH.lamCaseE static_matches) |]
    err
      -> error $ "Cannot call deriveHttpStatusCode on: " ++ show err

extract_names :: [TH.Con] -> Either TH.Con [TH.Name]
extract_names = mapM go
  where
    go :: TH.Con -> Either TH.Con TH.Name
    go = \case
      (TH.NormalC n []) -> Right n
      e                 -> Left e

parse_error_codes :: [TH.Name]
                  -> Either TH.Name [(TH.Name, TH.Q TH.Exp, T.Text)]
parse_error_codes = mapM go
  where

    do_parse = \n_txt ->
      let sts_tl = T.drop 3 n_txt
          code   = T.take 3 sts_tl
          msg    = T.drop 5 sts_tl
      in (code, msg)

    go :: TH.Name -> Either TH.Name (TH.Name, TH.Q TH.Exp, T.Text)
    go n = case Map.lookup code supported_http_status_map of
             Nothing -> Left n
             Just st -> Right (n, st, msg)
      where
        (code, msg) = do_parse $ (T.pack $ TH.nameBase n)

deriveIsFrontendErrorData :: TH.Name -> TH.Q [TH.Dec]
deriveIsFrontendErrorData appliedType = do
  info <- TH.reify appliedType
  case info of
    TH.TyConI (TH.DataD _ _ _ _ ctors _)
      -> case extract_names ctors of
           Left ctor   -> error $ "Only enum-like constructors supported: " ++ show ctor
           Right names -> fmap mconcat . sequence $ flip map names $ \n ->
               [d| instance IsFrontendErrorData $(TH.promotedT n) where
                     isFrontendErrorData _ = Dict |]
    err
      -> error $ "Cannot call deriveHttpStatusCode on: " ++ show err

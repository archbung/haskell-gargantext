
module Gargantext.API.Errors.Class where

import Control.Lens
import Gargantext.API.Admin.Auth.Types (AuthenticationError)

class HasAuthenticationError e where
  _AuthenticationError :: Prism' e AuthenticationError

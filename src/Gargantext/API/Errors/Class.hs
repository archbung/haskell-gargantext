
module Gargantext.API.Errors.Class where

import Control.Lens
import Crypto.JOSE.Error as Jose

class HasJoseError e where
  _JoseError :: Prism' e Jose.Error

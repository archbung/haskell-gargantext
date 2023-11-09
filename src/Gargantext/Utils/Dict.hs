{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gargantext.Utils.Dict where

import Prelude
import Data.Kind

-- A dictionary allowing us to treat constraints as first class values.
data Dict (c :: k -> Constraint) (a :: k) where
  Dict :: c a => Dict c a

deriving instance Show (Dict c a)

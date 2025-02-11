{-|
Module      : Gargantext.Core.Text.Terms.Multi.Lang.En
Description : English Grammar rules to group postag tokens.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Rule-based grammars are computed in this english module in order to group
the tokens into extracted terms.

-}


module Gargantext.Core.Text.Terms.Multi.Lang.En (groupTokens)
  where

import Gargantext.Prelude
import Gargantext.Core.Types ( POS(CC, IN, DT, NP, JJ), TokenTag )
import Gargantext.Core.Text.Terms.Multi.Group ( group2 )

------------------------------------------------------------------------
-- | Rule grammar to group tokens
groupTokens :: [TokenTag] -> [TokenTag]
groupTokens []    = []
groupTokens ntags = group2 NP NP
        --          $ group2 NP VB
        --          $ group2 NP IN
                  $ group2 IN DT
        --          $ group2 VB NP
                  $ group2 JJ NP
                  $ group2 JJ JJ
                  $ group2 JJ CC ntags

------------------------------------------------------------------------
--groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
--groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)
--groupNgrams ((x,_,"LOCATION"):(y,yy,"LOCATION"):xs)         = groupNgrams ((x <> " " <> y,yy,"LOCATION"):xs)
--
--groupNgrams (x:xs)                                          = (x:(groupNgrams xs))

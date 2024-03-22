{-|
Module      : Gargantext.Core.Text.Terms.Multi
Description : Multi Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Multi-terms are ngrams where n > 1.

-}

module Gargantext.Core.Text.Terms.Multi (multiterms, multiterms_rake, tokenTagsWith, tokenTags, cleanTextForNLP)
  where

import Data.Attoparsec.Text as DAT ( digit, space, notChar, string )
import Gargantext.Core (Lang(..), NLPServerConfig(..), PosTagAlgo(..))
import Gargantext.Core.Text.Terms.Multi.Lang.En qualified as En
import Gargantext.Core.Text.Terms.Multi.Lang.Fr qualified as Fr
import Gargantext.Core.Text.Terms.Multi.PosTagging ( corenlp, tokens2tokensTags )
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types ( PosSentences(_sentences), Sentence(_sentenceTokens) )
import Gargantext.Core.Text.Terms.Multi.RAKE (multiterms_rake)
import Gargantext.Core.Types ( POS(NP), Terms(Terms), TermsWithCount, TokenTag(TokenTag, _my_token_pos) )
import Gargantext.Core.Utils (groupWithCounts)
import Gargantext.Prelude
import Gargantext.Utils.SpacyNLP qualified as SpacyNLP
import Replace.Attoparsec.Text as RAT ( streamEdit )

-------------------------------------------------------------------
type NLP_API = Lang -> Text -> IO PosSentences

-------------------------------------------------------------------
multiterms :: NLPServerConfig -> Lang -> Text -> IO [TermsWithCount]
multiterms nsc l txt = do
  let txt' = cleanTextForNLP txt
  if txt' == ""
     then do
       printDebug "[G.C.T.Terms.Multi] becomes empty after cleanTextForNLP" txt
       pure []
     else do
       ret <- multiterms' tokenTag2terms l txt'
       pure $ groupWithCounts ret
    where
      multiterms' :: (TokenTag -> a) -> Lang -> Text -> IO [a]
      multiterms' f lang txt' = concat
                       <$> map (map f)
                       <$> map (filter (\t -> _my_token_pos t == Just NP))
                       <$> tokenTags nsc lang txt'

-------------------------------------------------------------------
tokenTag2terms :: TokenTag -> Terms
tokenTag2terms (TokenTag ws t _ _) =  Terms ws t

tokenTags :: NLPServerConfig -> Lang -> Text -> IO [[TokenTag]]
tokenTags (NLPServerConfig { server = CoreNLP, url }) EN txt = tokenTagsWith EN txt $ corenlp url
tokenTags (NLPServerConfig { server = CoreNLP, url }) FR txt = tokenTagsWith FR txt $ corenlp url
tokenTags (NLPServerConfig { server = Spacy, url }) l txt = do
  -- printDebug "NLP Debug" txt
  tokenTagsWith l txt $ SpacyNLP.nlp url
-- tokenTags FR txt = do
--   -- printDebug "[Spacy Debug]" txt
--   if txt == ""
--      then pure [[]]
--      else tokenTagsWith FR txt SpacyNLP.nlp
tokenTags _ l  _   = panicTrace $ "[G.C.T.T.Multi] Lang NLP API not implemented yet " <> (show l)

tokenTagsWith :: Lang -> Text -> NLP_API -> IO [[TokenTag]]
tokenTagsWith lang txt nlp = map (groupTokens lang)
                         <$> map tokens2tokensTags
                         <$> map _sentenceTokens
                         <$> _sentences
                         <$> nlp lang txt


---- | This function analyses and groups (or not) ngrams according to
----   specific grammars of each language.
groupTokens :: Lang -> [TokenTag] -> [TokenTag]
groupTokens EN = En.groupTokens
groupTokens FR = Fr.groupTokens
groupTokens _  = Fr.groupTokens

-- TODO: make tests here
cleanTextForNLP :: Text -> Text
cleanTextForNLP = unifySpaces . removeDigitsWith "-" . removeUrls
  where
    remove x = RAT.streamEdit x (const "")

    unifySpaces         = RAT.streamEdit (many DAT.space) (const " ")
    removeDigitsWith x  = remove (many DAT.digit *> DAT.string x <* many DAT.digit)

    removeUrls          = removeUrlsWith "http" . removeUrlsWith "www"
    removeUrlsWith w    = remove (DAT.string w *> many (DAT.notChar ' ') <* many DAT.space)

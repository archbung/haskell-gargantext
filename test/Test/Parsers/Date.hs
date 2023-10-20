{-|
Module      : Parsers.Date
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Test.Parsers.Date where


import Test.Hspec
import Test.QuickCheck

import Data.Time (ZonedTime(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Text (pack)

import Text.Parsec.Error (ParseError)
import Duckling.Time.Types (toRFC3339)

-----------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Core.Text.Corpus.Parsers.Date (dateSplit)
import Gargantext.Core.Text.Corpus.Parsers.Date.Parsec (fromRFC3339)
import Test.Parsers.Types

-----------------------------------------------------------

fromRFC3339Inv ::  Either ParseError ZonedTime -> Text
fromRFC3339Inv (Right z) = toRFC3339 z
fromRFC3339Inv (Left pe) = panic . pack $ show pe

testFromRFC3339 :: Spec
testFromRFC3339 = do
  describe "Test fromRFC3339: " $ do
    it "is the inverse of Duckling's toRFC3339" $ property $
      ((==) <*> (fromRFC3339 . fromRFC3339Inv)) . Right . looseZonedTimePrecision

      -- \x -> uncurry (==) $ (,) <*> (fromRFC3339 . fromRFC3339Inv) $ Right $ looseZonedTimePrecision x

      -- \x -> let e = Right x :: Either ParseError ZonedTime
      --       in fmap looseZonedTimePrecision e == (fromRFC3339 . fromRFC3339Inv ) (fmap looseZonedTimePrecision e)

testDateSplit :: Spec
testDateSplit = do
  describe "Test date split" $ do
    it "works for simple date parsing" $ do
      let utc = UTCTime { utctDay = fromOrdinalDate 2010 4
                        , utctDayTime = secondsToDiffTime 0 }
      dateSplit "2010-01-04" `shouldBe` Right (utc, (2010, 1, 4))

    it "throws error for year-month" $ do
      dateSplit "2010-01" `shouldSatisfy` isLeft

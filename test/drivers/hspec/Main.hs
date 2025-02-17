{-# LANGUAGE TypeApplications #-}

module Main where

import Gargantext.Prelude hiding (isInfixOf)

import Control.Monad
import Data.Text (isInfixOf)
import Shelly hiding (FilePath)
import System.IO
import System.Process
import Test.Hspec
import qualified Data.Text as T
import qualified Test.API                     as API
import qualified Test.Database.Operations     as DB


startCoreNLPServer :: IO ProcessHandle
startCoreNLPServer = do
  devNull <- openFile "/dev/null" WriteMode
  let p = proc "./startServer.sh" []
  (_, _, _, hdl) <- (createProcess $ p { cwd = Just "devops/coreNLP/stanford-corenlp-current"
                    , delegate_ctlc = True
                    , create_group = True
                    , std_out = UseHandle devNull
                    , std_err = UseHandle devNull
                    }) `catch` \e -> case e of
                                           _ | True <- "does not exist" `isInfixOf` (T.pack . show @SomeException $ e)
                                             -> fail $ "Cannot execute the 'startServer.sh' script. If this is the " <>
                                                       "first time you are running the tests, you have to run " <>
                                                       "cd devops/coreNLP && ./build.sh first. You have to run it only once, " <>
                                                       "and then you are good to go for the time being."
                                             | otherwise -> throwIO e
  pure hdl

stopCoreNLPServer :: ProcessHandle -> IO ()
stopCoreNLPServer = interruptProcessGroupOf

-- It's especially important to use Hspec for DB tests, because,
-- unlike 'tasty', 'Hspec' has explicit control over parallelism,
-- and it's important that DB tests are run according to a very
-- precise order, as they are not independent from each other.
-- Unfortunately it's not possibly to use the 'tasty-hspec' adapter
-- because by the time we get a 'TestTree' out of the adapter library,
-- the information about parallelism is lost.
--
-- /IMPORTANT/: For these tests to run correctly, you have to run
-- ./devops/coreNLP/build.sh first. You have to run it only /once/,
-- and then you are good to go for the time being.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  bracket startCoreNLPServer stopCoreNLPServer $ \_ -> hspec $ do
    API.tests
    DB.tests
    DB.nodeStoryTests

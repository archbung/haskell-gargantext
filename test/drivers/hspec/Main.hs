
module Main where

import Gargantext.Prelude

import Control.Exception
import Shelly hiding (FilePath)
import System.IO
import System.Process
import qualified Test.API                     as API
import qualified Test.Database.Operations     as DB

import Test.Hspec

startCoreNLPServer :: IO ProcessHandle
startCoreNLPServer = do
  devNull <- openFile "/dev/null" WriteMode
  let p = proc "./startServer.sh" []
  (_, _, _, hdl) <- createProcess $ p { cwd = Just "devops/coreNLP/stanford-corenlp-current"
                    , delegate_ctlc = True
                    , create_group = True
                    , std_out = UseHandle devNull
                    , std_err = UseHandle devNull
                    }
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

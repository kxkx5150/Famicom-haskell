module SpecHelper (
    run
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator
import           Emulator.Nes
import           Emulator.Mem
import           Test.Tasty.HUnit

run :: FilePath -> Emulator Word8 -> Word8 -> IO ()
run filename readResult expected = do
  rom  <- BS.readFile filename
  initNes rom $ do
    reset
    replicateM_ 300 step
    result <- readResult
    liftIO $ assertEqual "Return code" expected result

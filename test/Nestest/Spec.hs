
module Nestest.Spec where

import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator.Nes
import           Emulator.CPU           (runCpuT)
import           Emulator.Mem
import qualified Emulator.Util.Trace         as Trace
import           Emulator.Util.Util          (prettifyWordW)
import           Nestest.Parsing        (parseTrace)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec            (parse)

test :: TestTree
test = testCase "nestest" $ do
  rom <- BS.readFile "roms/tests/cpu/nestest/nestest.nes"
  lines <- lines <$> readFile "roms/tests/cpu/nestest/nestest.log"
  initNes rom $ do
    storeReg pc 0xC000
    emulate lines
  where
    emulate :: [String] -> Emulator ()
    emulate lines = do
      (_, trace) <- runCpuT
      case lines of
        [] -> pure ()
        (x:xs) -> case parse parseTrace "nestest.log" x of
          Left e -> liftIO $ assertFailure $ "Failed to parse " ++ show e
          Right nestestTrace -> do
            liftIO $ assertEqual ("Execution at " ++ prettifyWordW (Trace.pc trace)) nestestTrace trace
            emulate xs


module Emulator
  ( runEmulator
  , reset
  ) where

import           Emulator.Nes
import qualified Emulator.CPU                  as CPU

runEmulator :: Emulator ()
runEmulator = do
  cycles <- CPU.runCpu
  return ()

reset :: Emulator ()
reset = CPU.reset



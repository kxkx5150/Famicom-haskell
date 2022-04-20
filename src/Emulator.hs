module Emulator (
    start
  , reset
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Emulator.CPU        as CPU
import           Emulator.Nes

start :: Emulator ()
start = do
  cycles <- CPU.runCpu
  return ()

reset :: Emulator ()
reset = CPU.reset


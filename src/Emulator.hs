
module Emulator
  ( start
  , reset
  ) where

import qualified Emulator.CPU                  as CPU
import           Emulator.Mem

start :: Emulator ()
start = do
  cycles <- CPU.runCpu
  return ()

reset :: Emulator ()
reset = CPU.reset


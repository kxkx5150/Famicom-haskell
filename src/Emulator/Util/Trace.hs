{-# LANGUAGE DuplicateRecordFields #-}

module Emulator.Util.Trace
  ( Trace (..),
    renderTrace,
    mkTrace,
  )
where

import Data.Word
import Emulator.Mem
import Emulator.Nes
import Emulator.Opcode
import Text.Printf

data Trace = Trace
  { pc :: Word16,
    sp :: Word8,
    a :: Word8,
    x :: Word8,
    y :: Word8,
    p :: Word8,
    op :: Opcode,
    a0 :: Word8,
    a1 :: Word8,
    a2 :: Word8,
    cyc :: Int
  }
  deriving (Eq)

instance Show Trace where
  show = renderTrace

mkTrace :: Opcode -> Emulator Trace
mkTrace op = do
  pcv <- loadReg pc
  a0 <- readMemPort pcv
  a1 <- readMemPort (pcv + 1)
  a2 <- readMemPort (pcv + 2)
  spv <- loadReg sp
  av <- loadReg a
  xv <- loadReg x
  yv <- loadReg y
  pv <- loadReg p
  cycles <- loadReg cpuCycles
  let instrLength = len op
  let a1R = if instrLength < 2 then 0x0 else a1
  let a2R = if instrLength < 3 then 0x0 else a2
  pure (Trace pcv spv av xv yv pv op a0 a1R a2R ((cycles * 3) `rem` 341))

renderTrace :: Trace -> String
renderTrace (Trace pcv spv av xv yv pv (Opcode _ mnem _ _ _ _) a0 a1 a2 cyc) =
  executionPortion ++ registerPortion
  where
    a0R = printf "%02X" a0 :: String
    name = show mnem
    executionPortion = printf "%4X  %s        %s %28s" pcv a0R name ""
    registerPortion =
      printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d" av xv yv pv spv cyc

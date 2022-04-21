module Emulator.Mem
  ( readCpuMemory8
  , readMemW
  , writeCpuMemory8
  ) where

import qualified Data.Vector.Storable.Mutable  as VUM
import           Data.Word
import qualified Emulator.Mapper               as Mapper
import           Emulator.Nes
import           Emulator.Util


readCpuMemory8 :: Word16 -> Emulator Word8
readCpuMemory8 addr
  | addr < 0x2000  = readMem addr
  | addr < 0x4000  = pure 0
  | addr == 0x4014 = pure 0
  | addr == 0x4015 = pure 0
  | addr == 0x4016 = pure 0
  | addr == 0x4017 = pure 0
  | addr < 0x6000  = pure 0
  | addr >= 0x6000 = readMapper addr
  | otherwise      = error $ "memory read error at " ++ show addr ++ "!"

writeCpuMemory8 :: Word16 -> Word8 -> Emulator ()
writeCpuMemory8 addr value
  | addr < 0x2000                    = writeMem addr value
  | addr < 0x4000                    = pure ()
  | addr == 0x4014                   = pure ()
  | addr == 0x4016                   = pure ()
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = pure ()
  | addr >= 0x6000                   = writeMapper addr value
  | otherwise = error $ "memory write error at " ++ show addr ++ "!"

readMem :: Word16 -> Emulator Word8
readMem addr =
  with cpu $ \cpu -> VUM.read (ram cpu) (fromIntegral addr `rem` 0x0800)

readMemW :: Word16 -> Emulator Word16
readMemW addr = do
  lo <- readCpuMemory8 addr
  hi <- readCpuMemory8 (addr + 1)
  pure $ makeW16 lo hi

writeMem :: Word16 -> Word8 -> Emulator ()
writeMem addr v =
  with cpu $ \cpu -> VUM.write (ram cpu) (fromIntegral addr `rem` 0x0800) v

readMapper :: Word16 -> Emulator Word8
readMapper addr = with mapper $ \mapper -> Mapper.read mapper addr

writeMapper :: Word16 -> Word8 -> Emulator ()
writeMapper addr value =
  with mapper $ \mapper -> Mapper.write mapper addr value

